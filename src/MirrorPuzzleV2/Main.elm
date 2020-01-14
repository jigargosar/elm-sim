module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Direction8 as Dir exposing (Direction8)
import MirrorPuzzleV2.Grid as Grid exposing (Pos)
import MirrorPuzzleV2.GridShape as GS
import MirrorPuzzleV2.Rect as Rect exposing (Rect)
import Playground exposing (..)
import Playground.Extra exposing (..)
import PointFree exposing (whenTrue)
import Set exposing (Set)



-- Puzzle Data Model


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


type alias Grid =
    Grid.Grid Cell


mirror : Int -> Cell
mirror =
    Dir.fromInt >> Mirror


sourceWithMirror : Int -> Cell
sourceWithMirror =
    Dir.fromInt >> SourceWithMirror


initialGrid : Grid
initialGrid =
    Grid.filled 5 5 Empty
        |> insert ( 1, 2 ) (sourceWithMirror 1)
        |> insert ( 2, 3 ) (mirror 7)
        |> insert ( 3, 2 ) Destination
        |> insert ( 4, 4 ) (sourceWithMirror -3)
        |> insert ( 0, 0 ) Destination
        |> insert ( 1, 1 ) (sourceWithMirror 1)
        |> insert ( 1, 1 ) Source


insert : ( Int, Int ) -> Cell -> Grid -> Grid
insert =
    Grid.insert


computeLitDestinationPosSet : Grid -> Set Pos
computeLitDestinationPosSet grid =
    gridToLightPaths grid
        |> List.filterMap List.head
        |> List.foldl
            (\pos ->
                if Grid.get pos grid == Just Destination then
                    Set.insert pos

                else
                    identity
            )
            Set.empty


type alias LightPath =
    List Pos


gridToLightPaths : Grid -> List LightPath
gridToLightPaths grid =
    let
        accumLightPos : Direction8 -> Pos -> List Pos -> List Pos
        accumLightPos dir pos acc =
            let
                nextPos =
                    Dir.stepPos dir pos

                accumInDir newDir =
                    accumLightPos newDir nextPos (nextPos :: acc)
            in
            case Grid.get nextPos grid of
                Nothing ->
                    acc

                Just cell ->
                    case cell of
                        Source ->
                            accumInDir dir

                        SourceWithMirror _ ->
                            acc

                        Destination ->
                            nextPos :: acc

                        Mirror direction ->
                            accumInDir direction

                        Empty ->
                            accumInDir dir
    in
    Grid.foldl
        (\pos cell ->
            case cell of
                SourceWithMirror dir ->
                    accumLightPos dir pos [ pos ]
                        |> (::)

                _ ->
                    identity
        )
        []
        grid



-- Puzzle Grid View


toBgShape : Number -> Shape
toBgShape cz =
    group
        [ rectangle black cz cz |> scale 0.95
        , rectangle white cz cz |> scale 0.9
        ]


sourceShape : Number -> Shape
sourceShape cz =
    rectangle orange cz cz


mirrorShape : Number -> Direction8 -> Shape
mirrorShape cz dir =
    group
        [ group [ oval green (cz / 2) cz |> moveLeft (cz / 6) ]
            |> rotate (Dir.toDegrees dir)
        , circle lightPurple 10
        ]
        |> scale 0.9


cellToShape : Number -> Cell -> Shape
cellToShape cz cell =
    case cell of
        Source ->
            sourceShape cz

        Mirror dir ->
            mirrorShape cz dir

        SourceWithMirror dir ->
            group
                [ sourceShape cz
                , mirrorShape cz dir
                ]

        Destination ->
            circle blue (cz / 2)

        Empty ->
            noShape


pathToShape : GS.GridShape a -> List Pos -> Shape
pathToShape gs =
    List.map (GS.posToScreen gs)
        >> (\path -> List.map2 (line red 5) path (List.drop 1 path))
        >> group


viewGrid : Time -> Grid -> Shape
viewGrid time grid =
    let
        cz =
            100

        gs =
            GS.init cz cz grid

        litDestinationPosSet =
            computeLitDestinationPosSet grid

        renderCell pos cell =
            cellToShape cz cell
                |> whenTrue (Set.member pos litDestinationPosSet) (blink time)
                |> scale 0.85

        lightPaths : List LightPath
        lightPaths =
            gridToLightPaths grid
    in
    group
        [ GS.fill (toBgShape cz) gs
        , GS.render renderCell gs
        , lightPaths
            |> List.map (pathToShape gs)
            |> group
            |> GS.move gs
        ]
        |> scale 1.5



-- LevelSelect


type alias LevelButtons =
    { top : Number
    , list : List Rect
    }


initLevelButtons : Screen -> Int -> LevelButtons
initLevelButtons screen count =
    let
        lh =
            (screen.height * 0.7) / toFloat count

        hScale =
            0.8

        top =
            (toFloat count * lh - lh) / 2

        toY : Int -> Number
        toY n =
            top - (toFloat n * lh)

        height =
            lh * hScale

        width =
            150
    in
    { top = top
    , list =
        List.range 0 (count - 1)
            |> List.map
                (\n ->
                    Rect.fromXYWH 0 (toY n) width height
                )
    }


levelButtonIdxFromMouse : Mouse -> LevelButtons -> Maybe Int
levelButtonIdxFromMouse mouse lbs =
    lbs.list
        |> List.Extra.findIndex (mouseInRect mouse)


renderLevelButtons : Mouse -> LevelButtons -> Shape
renderLevelButtons mouse lbs =
    lbs.list
        |> List.indexedMap
            (\levelIdx rect ->
                renderButton
                    mouse
                    ("Level " ++ String.fromInt (levelIdx + 1))
                    rect
            )
        |> group


renderButton : Mouse -> String -> Rect -> Shape
renderButton mouse text rect =
    let
        ( x, y ) =
            Rect.center rect
    in
    buttonShape (mouseInRect mouse rect)
        (Rect.dimensions rect)
        text
        |> move x y


buttonShape : Bool -> ( Number, Number ) -> String -> Shape
buttonShape hover ( w, h ) text =
    let
        thickness =
            3
    in
    [ rectangle black w h
    , rectangle
        (if hover then
            lightPurple

         else
            white
        )
        (w - thickness)
        (h - thickness)
    , words black text
    ]
        |> group


viewLevelSelect : Mouse -> LevelButtons -> List Shape
viewLevelSelect mouse lbs =
    [ words black "Select Level"
        |> scale 1.5
        |> moveUp lbs.top
        |> moveUp 60
    , renderLevelButtons mouse lbs
    ]



-- Scene


type Scene
    = Puzzle Grid
    | Intro
    | LevelSelect Int



-- Game


type alias Mem =
    { scene : Scene }


initialPuzzle =
    Puzzle initialGrid


initialLevelSelect =
    LevelSelect 10


init : Mem
init =
    { scene = initialLevelSelect }


update : Computer -> Mem -> Mem
update { mouse, screen } mem =
    case mem.scene of
        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons screen levelCount
            in
            case ( mouse.click, levelButtonIdxFromMouse mouse lbs ) of
                ( True, Just _ ) ->
                    { mem | scene = initialPuzzle }

                _ ->
                    mem

        Intro ->
            if mouse.click then
                { mem | scene = initialLevelSelect }

            else
                mem

        Puzzle _ ->
            if mouse.click && mouseInRect mouse (backButtonRectFromScreen screen) then
                { mem | scene = Intro }

            else
                mem


viewPuzzle : Computer -> Grid -> List Shape
viewPuzzle { mouse, time, screen } grid =
    [ viewGrid time grid
    , renderButton mouse "Back" (backButtonRectFromScreen screen)
    ]


backButtonRectFromScreen : Screen -> Rect
backButtonRectFromScreen screen =
    Rect.fromXYWH (screen.left + 100) (screen.top - 50) 100 30


mouseInRect : Mouse -> Rect -> Bool
mouseInRect mouse rect =
    Rect.contains ( mouse.x, mouse.y ) rect


view : Computer -> Mem -> List Shape
view computer mem =
    case mem.scene of
        Puzzle grid ->
            viewPuzzle computer grid

        Intro ->
            [ words black "Tap To Start" ]

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons computer.screen levelCount
            in
            viewLevelSelect computer.mouse lbs


main =
    game view update init
