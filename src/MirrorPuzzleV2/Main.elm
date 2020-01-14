module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Direction8 as Dir exposing (Direction8)
import MirrorPuzzleV2.Grid as Grid exposing (Pos)
import MirrorPuzzleV2.GridShape as GS
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
            GS.init cz grid

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
    { count : Int
    , top : Number
    , width : Number
    , height : Number
    , toY : Int -> Number
    , list : List LevelButton
    }


levelButtons : Float -> Int -> LevelButtons
levelButtons lh count =
    let
        hScale =
            0.8

        top =
            toFloat count * lh / 2

        toY : Int -> Number
        toY n =
            top - (toFloat n * lh)

        height =
            lh * hScale

        width =
            150
    in
    { count = count
    , top = top
    , width = width
    , height = height
    , toY = toY
    , list =
        List.range 0 (count - 1)
            |> List.map
                (\n ->
                    { x = 0
                    , y = toY n
                    , width = width
                    , height = height
                    , levelIdx = n
                    }
                )
    }


levelButtonIdxFromMouse : Mouse -> LevelButtons -> Maybe Int
levelButtonIdxFromMouse { x, y } lbs =
    lbs.list
        |> List.Extra.find (\lb -> hitTest ( x, y ) ( ( lb.x, lb.y ), ( lb.width, lb.height ) ))
        |> Maybe.map .levelIdx


hitTest : ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) ) -> Bool
hitTest ( px, py ) ( ( x, y ), ( w, h ) ) =
    let
        ( minX, maxX ) =
            ( x - w / 2, x + w / 2 )

        ( minY, maxY ) =
            ( y - h / 2, y + h / 2 )
    in
    px > minX && px < maxX && py > minY && py < maxY


renderLevelButtons : Mouse -> LevelButtons -> Shape
renderLevelButtons mouse lbs =
    let
        maybeHoveredIdx =
            levelButtonIdxFromMouse mouse lbs

        isHovered levelIdx =
            Just levelIdx == maybeHoveredIdx
    in
    lbs.list
        |> List.map
            (\{ x, y, width, height, levelIdx } ->
                buttonShape (isHovered levelIdx) width height ("Level " ++ String.fromInt (levelIdx + 1))
                    |> move x y
            )
        |> group


type alias LevelButton =
    { x : Float
    , y : Float
    , width : Number
    , height : Number
    , levelIdx : Int
    }


mapLevelButtons : Mouse -> (LevelButton -> b) -> LevelButtons -> List b
mapLevelButtons mouse func { count, toY, width, height } =
    List.range 0 (count - 1)
        |> List.map
            (\n ->
                let
                    y =
                        toY n
                in
                func { x = 0, y = y, width = width, height = height, levelIdx = n }
            )


buttonShape hover w h text =
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
    | LevelSelect LevelButtons



-- Game


type alias Mem =
    { scene : Scene }


initialPuzzle =
    Puzzle initialGrid


initialLevelSelect =
    LevelSelect (levelButtons 40 10)


init : Mem
init =
    { scene = initialLevelSelect }


update : Computer -> Mem -> Mem
update { mouse } mem =
    case mem.scene of
        LevelSelect lbs ->
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
            if mouse.click then
                { mem | scene = Intro }

            else
                mem


view : Computer -> Mem -> List Shape
view { time, mouse } mem =
    case mem.scene of
        Puzzle grid ->
            [ viewGrid time grid ]

        Intro ->
            [ words black "Tap To Start" ]

        LevelSelect lbs ->
            viewLevelSelect mouse lbs


main =
    game view update init
