module MirrorPuzzleV2.PuzzleGrid exposing
    ( PuzzleGrid
    , initCellT
    , initialGrid
    , isSolved
    , updatePuzzleGrid
    , viewPuzzleGrid
    )

import Number2 as NT exposing (Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid
import PointFree exposing (whenTrue)
import Set exposing (Set)


type PuzzleCell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


type alias PuzzleGrid =
    Grid.Grid PuzzleCell


mirror : Int -> PuzzleCell
mirror =
    Dir.fromInt >> Mirror


sourceWithMirror : Int -> PuzzleCell
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


updatePuzzleGrid : Computer -> PuzzleGrid -> PuzzleGrid
updatePuzzleGrid { screen, mouse } grid =
    let
        ct =
            initCellT screen grid

        pos =
            ct.fromView ( mouse.x, mouse.y )
    in
    case ( mouse.click, Grid.get pos grid ) of
        ( True, Just cell ) ->
            let
                ins a =
                    Grid.insert pos a grid
            in
            case cell of
                Mirror dir ->
                    ins (Mirror (Dir.rotate 1 dir))

                SourceWithMirror dir ->
                    ins (SourceWithMirror (Dir.rotate 1 dir))

                _ ->
                    grid

        _ ->
            grid


destinationSet : PuzzleGrid -> Set Int2
destinationSet =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


isSolved : PuzzleGrid -> Bool
isSolved grid =
    destinationSet grid == listDestinations grid


listDestinations : PuzzleGrid -> Set Int2
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
    List Int2


gridToLightPaths : PuzzleGrid -> List LightPath
gridToLightPaths grid =
    let
        accumLightPos : Direction8 -> Int2 -> List Int2 -> List Int2
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
    group [ gridCellShapes time ct grid |> group, lightPathsShape ]



-- Puzzle Ligh Path View


viewPath : CellTransform -> List Int2 -> Shape
viewPath ct =
    List.map ct.toView
        >> (\path -> List.map2 (line red 5) path (List.drop 1 path))
        >> group



-- Puzzle Cell View


gridCellShapes : Time -> CellTransform -> PuzzleGrid -> List Shape
gridCellShapes time ct grid =
    let
        litDest =
            listDestinations grid

        toCellView ( pos, cell ) =
            cellShape time ct litDest pos cell
    in
    grid |> Grid.toList |> List.map toCellView


cellShape : Time -> CellTransform -> Set Int2 -> Int2 -> PuzzleCell -> Shape
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


cellContentShapes : Time -> Number -> Set comparable -> comparable -> PuzzleCell -> List Shape
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


mirrorShape : Float -> Direction8 -> Shape
mirrorShape width dir =
    group
        [ group [ oval green (width / 2) width |> moveLeft (width / 6) ]
            |> rotate (Dir.toDegrees dir)
        , circle lightPurple 10
        ]
        |> scale 0.9


destinationShape : Time -> Float -> Bool -> Shape
destinationShape time width isLit =
    circle blue (width / 2)
        |> whenTrue isLit (blink time)
