module Playground.Grid exposing
    ( Grid
    , dimensions
    , filled
    , foldl
    , get
    , insert
    , isValid
    , map
    , positions
    , toList
    , update
    , update2
    , values
    )

import Dict exposing (Dict)
import Number2 exposing (Int2)
import PointFree exposing (when)


type Grid a
    = Grid Int Int (Dict Int2 a)


filled : Int -> Int -> a -> Grid a
filled w h a =
    foldWH (\pos -> Dict.insert pos a) Dict.empty w h |> Grid w h


get : Int2 -> Grid a -> Maybe a
get pos =
    toDict >> Dict.get pos


insert : Int2 -> b -> Grid b -> Grid b
insert pos a =
    when (isValid pos)
        (mapDict (Dict.insert pos a))


dimensions : Grid a -> ( Int, Int )
dimensions (Grid w h _) =
    ( w, h )


isValid : Int2 -> Grid a -> Bool
isValid ( x, y ) (Grid w h _) =
    isValidIdx w x && isValidIdx h y


map : (Int2 -> a -> b) -> Grid a -> Grid b
map func =
    mapDict (Dict.map func)


values : Grid a -> List a
values =
    toDict >> Dict.values


positions : Grid a -> List Int2
positions =
    toDict >> Dict.keys


foldl : (Int2 -> a -> b -> b) -> b -> Grid a -> b
foldl func acc =
    toDict >> Dict.foldl func acc


toList : Grid a -> List ( Int2, a )
toList =
    toDict >> Dict.toList


update : Int2 -> (a -> a) -> Grid a -> Maybe (Grid a)
update gIdx func grid =
    get gIdx grid
        |> Maybe.map (\cell -> insert gIdx (func cell) grid)


update2 : Int2 -> Int2 -> (b -> b -> ( b, b )) -> Grid b -> Maybe (Grid b)
update2 gIdxA gIdxB func grid =
    let
        insertCells ( newCellA, newCellB ) =
            grid
                |> insert gIdxA newCellA
                |> insert gIdxB newCellB
    in
    Maybe.map2
        (\cellA cellB -> func cellA cellB |> insertCells)
        (get gIdxA grid)
        (get gIdxB grid)


toDict : Grid a -> Dict Int2 a
toDict (Grid _ _ dict) =
    dict


isValidIdx : number -> number -> Bool
isValidIdx len idx =
    idx >= 0 && idx < len


mapDict : (Dict Int2 a -> Dict Int2 b) -> Grid a -> Grid b
mapDict func (Grid w h dict) =
    func dict |> Grid w h


foldWH : (Int2 -> b -> b) -> b -> Int -> Int -> b
foldWH func acc0 w h =
    List.range 0 (h - 1)
        |> List.foldl
            (\y acc1 ->
                List.range 0 (w - 1)
                    |> List.foldl (\x -> func ( x, y )) acc1
            )
            acc0
