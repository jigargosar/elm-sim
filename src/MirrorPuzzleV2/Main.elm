module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import Number2 as NT exposing (Float2, Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid exposing (Pos)
import PointFree exposing (whenTrue)
import Set exposing (Set)



-- Puzzle Grid Model


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


type alias PuzzleGrid =
    Grid.Grid Cell


mirror : Int -> Cell
mirror =
    Dir.fromInt >> Mirror


sourceWithMirror : Int -> Cell
sourceWithMirror =
    Dir.fromInt >> SourceWithMirror


initialGrid : PuzzleGrid
initialGrid =
    let
        insert =
            Grid.insert
    in
    Grid.filled 5 5 Empty
        |> insert ( 1, 2 ) (sourceWithMirror 1)
        |> insert ( 2, 3 ) (mirror 7)
        |> insert ( 3, 2 ) Destination
        |> insert ( 4, 4 ) (sourceWithMirror -3)
        |> insert ( 0, 0 ) Destination
        |> insert ( 1, 1 ) (sourceWithMirror 1)
        |> insert ( 1, 1 ) Source


destinationSet : PuzzleGrid -> Set Pos
destinationSet =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


isSolved : PuzzleGrid -> Bool
isSolved grid =
    destinationSet grid == listDestinations grid


listDestinations : PuzzleGrid -> Set Pos
listDestinations grid =
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


gridToLightPaths : PuzzleGrid -> List LightPath
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


initCellT : Screen -> PuzzleGrid -> CellTransform
initCellT screen grid =
    let
        viewD =
            NT.scale 0.8 ( screen.width, screen.height )
    in
    CT.fromViewD viewD (Grid.dimensions grid)


viewPuzzleGrid : Computer -> PuzzleGrid -> Shape
viewPuzzleGrid { time, screen } grid =
    let
        ct =
            initCellT screen grid

        lightPathsShape =
            grid
                |> gridToLightPaths
                |> List.map (viewPath ct)
                |> group
    in
    group [ viewPuzzleCells time ct grid, lightPathsShape ]



-- Puzzle Ligh Path View


viewPath : CellTransform -> List Pos -> Shape
viewPath ct =
    List.map ct.toView
        >> (\path -> List.map2 (line red 5) path (List.drop 1 path))
        >> group



-- Puzzle Cell View


viewPuzzleCells : Time -> CellTransform -> PuzzleGrid -> Shape
viewPuzzleCells time ct grid =
    grid
        |> gridToCellViewList ct
        |> List.map (viewCell time ct.cellSize)
        |> group


gridToCellViewList : CellTransform -> PuzzleGrid -> List ( Float2, List CellForm )
gridToCellViewList ct grid =
    let
        litDest =
            listDestinations grid

        cellToForm pos cell =
            case cell of
                Source ->
                    [ SRC ]

                Destination ->
                    [ DEST (Set.member pos litDest) ]

                SourceWithMirror dir ->
                    [ SRC, MIR dir ]

                Mirror dir ->
                    [ MIR dir ]

                Empty ->
                    []

        toCellView ( pos, cell ) =
            ( ct.toView pos, cellToForm pos cell )
    in
    grid |> Grid.toList |> List.map toCellView


type CellForm
    = SRC
    | MIR Direction8
    | DEST Bool


gridCellShapes : Time -> CellTransform -> PuzzleGrid -> List Shape
gridCellShapes time ct grid =
    let
        litDest =
            listDestinations grid

        toCellView ( pos, cell ) =
            cellShape time ct litDest pos cell
    in
    grid |> Grid.toList |> List.map toCellView


cellShape : Time -> CellTransform -> Set Int2 -> Int2 -> Cell -> Shape
cellShape time ct litDest pos cell =
    let
        bg : Shape
        bg =
            group
                [ rectangle black width width |> scale 0.95
                , rectangle white width width |> scale 0.9
                ]

        width =
            ct.cellSize
    in
    cellContentShapes time width litDest pos cell
        |> List.map (scale 0.8)
        |> (::) bg
        |> group
        |> move2 (ct.toView pos)


cellContentShapes : Time -> Number -> Set comparable -> comparable -> Cell -> List Shape
cellContentShapes time width litDest pos cell =
    case cell of
        Source ->
            [ srcShape width ]

        Destination ->
            [ destinationShape time width (Set.member pos litDest) ]

        SourceWithMirror dir ->
            [ srcShape width, mirrorShape width dir ]

        Mirror dir ->
            [ mirrorShape width dir ]

        Empty ->
            []


srcShape : Number -> Shape
srcShape width =
    rectangle orange width width


mirrorShape width dir =
    group
        [ group [ oval green (width / 2) width |> moveLeft (width / 6) ]
            |> rotate (Dir.toDegrees dir)
        , circle lightPurple 10
        ]
        |> scale 0.9


destinationShape time width isLit =
    circle blue (width / 2)
        |> whenTrue isLit (blink time)


cellFormToShape : Time -> Number -> CellForm -> Shape
cellFormToShape time cz form =
    case form of
        SRC ->
            rectangle orange cz cz

        MIR dir ->
            group
                [ group [ oval green (cz / 2) cz |> moveLeft (cz / 6) ]
                    |> rotate (Dir.toDegrees dir)
                , circle lightPurple 10
                ]
                |> scale 0.9

        DEST lit ->
            circle blue (cz / 2)
                |> whenTrue lit (blink time)


viewCell : Time -> Float -> ( Float2, List CellForm ) -> Shape
viewCell time width ( pos, cellForms ) =
    let
        bg =
            group
                [ rectangle black width width |> scale 0.95
                , rectangle white width width |> scale 0.9
                ]
    in
    List.map (cellFormToShape time width >> scale 0.8) cellForms
        |> (::) bg
        |> group
        |> move2 pos



-- LevelSelect


type alias LevelButtons =
    { top : Number
    , list : List Box
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
                    Box.withWH width height
                        |> Box.moveY (toY n)
                )
    }


levelButtonIdxFromMouse : Mouse -> LevelButtons -> Maybe Int
levelButtonIdxFromMouse mouse lbs =
    lbs.list
        |> List.Extra.findIndex (Box.containsXY mouse)


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


renderButton : Mouse -> String -> Box -> Shape
renderButton mouse text rect =
    buttonShape (Box.containsXY mouse rect)
        (Box.dimensions rect)
        text
        |> move2 (Box.center rect)


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
    = Intro
    | LevelSelect Int
    | PuzzleScene Puzzle


type alias Puzzle =
    { grid : PuzzleGrid }



-- Game


type alias Mem =
    { scene : Scene }


initialPuzzleScene =
    PuzzleScene { grid = initialGrid }


initialLevelSelect =
    LevelSelect 10


init : Mem
init =
    { scene = initialPuzzleScene }


updateMem : Computer -> Mem -> Mem
updateMem computer mem =
    let
        { mouse, screen } =
            computer
    in
    case mem.scene of
        Intro ->
            if mouse.click then
                { mem | scene = initialLevelSelect }

            else
                mem

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons screen levelCount
            in
            case ( mouse.click, levelButtonIdxFromMouse mouse lbs ) of
                ( True, Just _ ) ->
                    { mem | scene = initialPuzzleScene }

                _ ->
                    mem

        PuzzleScene puzzle ->
            if mouse.click then
                if Box.containsXY mouse (initBackButtonBox screen) then
                    { mem | scene = initialLevelSelect }

                else
                    { mem | scene = PuzzleScene (updatePuzzleScene computer puzzle) }

            else
                mem


updatePuzzleScene : Computer -> Puzzle -> Puzzle
updatePuzzleScene { screen, mouse } model =
    let
        grid =
            model.grid

        ct =
            initCellT screen grid

        pos =
            ct.fromView ( mouse.x, mouse.y )
    in
    case Grid.get pos grid of
        Just cell ->
            let
                ins a =
                    { model | grid = Grid.insert pos a grid }
            in
            case cell of
                Mirror dir ->
                    ins (Mirror (Dir.rotate 1 dir))

                SourceWithMirror dir ->
                    ins (SourceWithMirror (Dir.rotate 1 dir))

                _ ->
                    model

        Nothing ->
            model


viewPuzzleScene : Computer -> Puzzle -> List Shape
viewPuzzleScene computer { grid } =
    let
        { mouse, time, screen } =
            computer
    in
    [ viewPuzzleGrid computer grid
    , if isSolved grid then
        words black "puzzle solved"
            |> moveY screen.top
            |> moveDown 50

      else
        noShape
    , renderButton mouse "Back" (initBackButtonBox screen)
    ]


initBackButtonBox : Screen -> Box
initBackButtonBox screen =
    Box.withWH 100 30
        |> Box.move ( screen.left, screen.top )
        |> Box.moveDown 50
        |> Box.moveRight 100


view : Computer -> Mem -> List Shape
view computer mem =
    case mem.scene of
        Intro ->
            [ words black "Tap To Start" ]

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons computer.screen levelCount
            in
            viewLevelSelect computer.mouse lbs

        PuzzleScene puzzle ->
            viewPuzzleScene computer puzzle


main =
    game view updateMem init
