module MirrorPuzzleV2.Main exposing (main)

import Dict exposing (Dict)
import Playground exposing (..)



-- Puzzle Data Model


type MirrorDirection
    = MirrorDirection


type Cell
    = Source
      --| Destination
      --| SourceWithMirror MirrorDirection
    | Mirror Float


type Grid
    = Grid Int Int (Dict ( Int, Int ) Cell)


initialMirror : Cell
initialMirror =
    Mirror (45 * 1)


initialGrid : Grid
initialGrid =
    Grid 5 5 Dict.empty
        |> insert ( 1, 3 ) Source
        |> insert ( 3, 3 ) initialMirror


insert : ( Int, Int ) -> Cell -> Grid -> Grid
insert pos cell ((Grid w h dict) as grid) =
    if isPositionInGrid pos grid then
        Dict.insert pos cell dict |> Grid w h

    else
        grid


isPositionInGrid : ( Int, Int ) -> Grid -> Bool
isPositionInGrid ( x, y ) (Grid w h _) =
    isValidIdx w x && isValidIdx h y


isValidIdx : number -> number -> Bool
isValidIdx len idx =
    idx >= 0 && idx < len


gridToMaybeCellList : Grid -> List ( ( Int, Int ), Maybe Cell )
gridToMaybeCellList (Grid w h dict) =
    List.range 0 (h - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (w - 1)
                    |> List.map (\x -> ( ( x, y ), Dict.get ( x, y ) dict ))
            )



-- Puzzle Grid View


bgShape : Number -> Shape
bgShape cz =
    group
        [ rectangle black cz cz |> scale 0.95
        , rectangle white cz cz |> scale 0.9
        ]


cellToShape : Number -> Cell -> Shape
cellToShape cz cell =
    group
        [ bgShape cz
        , (case cell of
            Source ->
                rectangle orange cz cz

            Mirror angDeg ->
                group
                    [ group [ oval green (cz / 2) cz |> moveLeft (cz / 6) ]
                        |> rotate angDeg
                    , circle lightPurple 10
                    ]
          )
            |> scale 0.8
        ]


viewMaybeCell : Number -> ( ( Int, Int ), Maybe Cell ) -> Shape
viewMaybeCell cz ( ( x, y ), maybeCell ) =
    maybeCell
        |> Maybe.map (cellToShape cz)
        |> Maybe.withDefault (bgShape cz)
        |> move (toFloat x * cz) (toFloat y * cz)


viewGrid : Grid -> Shape
viewGrid grid =
    let
        cz =
            100

        ( dy, dx ) =
            let
                ( w, h ) =
                    let
                        (Grid iw ih _) =
                            grid
                    in
                    ( toFloat iw * cz, toFloat ih * cz )
            in
            ( (cz - w) / 2, (cz - h) / 2 )
    in
    group
        [ gridToMaybeCellList grid
            |> List.map (viewMaybeCell cz)
            |> group
            |> move dx dy
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
view _ mem =
    [ viewGrid mem.grid ]


noShape =
    group []


main =
    game view update init
