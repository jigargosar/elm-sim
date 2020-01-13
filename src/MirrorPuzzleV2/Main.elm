module MirrorPuzzleV2.Main exposing (main)

import MirrorPuzzleV2.Direction8 as Dir exposing (Direction8)
import MirrorPuzzleV2.Grid as Grid exposing (Pos)
import MirrorPuzzleV2.GridShape as GS
import Playground exposing (..)
import Playground.Extra exposing (..)
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


cellToShape : Number -> Cell -> Shape
cellToShape cz cell =
    case cell of
        Source ->
            sourceShape cz

        Mirror angDeg ->
            mirrorShape cz angDeg

        SourceWithMirror angDeg ->
            group
                [ sourceShape cz
                , mirrorShape cz angDeg
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

        litDestinationsPosSet =
            computeLitDestinationPosSet grid

        renderCell pos cell =
            cellToShape cz cell
                |> (if Set.member pos litDestinationsPosSet then
                        blink time

                    else
                        identity
                   )
                |> scale 0.8

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



-- Game Scaffold


type alias Mem =
    { grid : Grid }


init : Mem
init =
    { grid = initialGrid }


update : Computer -> Mem -> Mem
update _ mem =
    mem


view : Computer -> Mem -> List Shape
view { time } mem =
    [ viewGrid time mem.grid ]



--noinspection ElmUnusedSymbol


main =
    game view update init
