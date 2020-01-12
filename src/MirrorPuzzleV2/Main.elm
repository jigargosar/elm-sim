module MirrorPuzzleV2.Main exposing (main)

import Dict exposing (Dict)
import Playground exposing (..)



-- Puzzle Data Model


type MirrorDirection
    = MirrorDirection


type Cell
    = Source
    | Destination
    | SourceWithMirror MirrorDirection
    | Mirror MirrorDirection


type Grid
    = Grid Int Int (Dict ( Int, Int ) Cell)


initialMirror : Cell
initialMirror =
    Mirror MirrorDirection


initialGrid : Grid
initialGrid =
    Grid 5 5 Dict.empty
        |> insert ( 1, 3 ) Source
        |> insert ( 3, 3 ) initialMirror


insert pos cell ((Grid w h dict) as grid) =
    if isPositionInGrid pos grid then
        Dict.insert pos cell dict |> Grid w h

    else
        grid


isPositionInGrid ( x, y ) (Grid w h _) =
    isValidIdx w x && isValidIdx h y


isValidIdx len idx =
    idx >= 0 && idx < len


gridToListAllPos : Grid -> List ( ( Int, Int ), Maybe Cell )
gridToListAllPos (Grid w h dict) =
    List.range 0 (h - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (w - 1)
                    |> List.map (\x -> ( ( x, y ), Dict.get ( x, y ) dict ))
            )



-- Puzzle Grid View


viewGrid : Grid -> Shape
viewGrid grid =
    let
        cz =
            100

        ( w, h ) =
            let
                (Grid iw ih _) =
                    grid
            in
            ( toFloat iw * cz, toFloat ih * cz )

        ( dy, dx ) =
            ( (cz - w) / 2, (cz - h) / 2 )

        cellRect color =
            rectangle color cz cz

        scaledCellRect color s =
            cellRect color |> scale s

        bgWith shape =
            group
                [ scaledCellRect black 0.95
                , scaledCellRect white 0.9
                , shape |> scale 0.8
                ]

        cellToShape cell =
            case cell of
                Source ->
                    cellRect orange

                _ ->
                    noShape

        maybeCellToShape maybeCell =
            case maybeCell of
                Just cell ->
                    bgWith (cellToShape cell)

                Nothing ->
                    bgWith noShape

        viewMaybeCell ( ( x, y ), maybeCell ) =
            maybeCellToShape maybeCell
                |> move (toFloat x * cz) (toFloat y * cz)
    in
    group
        [ gridToListAllPos grid
            |> List.map viewMaybeCell
            |> group
            |> move dx dy
        ]
        |> scale 1.5


noShape =
    group []



-- Game Scaffold


type alias Mem =
    { grid : Grid }


init : Mem
init =
    { grid = initialGrid }


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    [ viewGrid mem.grid ]


main =
    game view update init
