module Playground.Grid exposing
    ( Grid
    , dimensions
    , filled
    , foldl
    , get
    , insert
    , isValid
    , map
    , mapCellAt2
    , positions
    , toList
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


mapCellAt2 : (b -> b -> Maybe ( b, b )) -> Int2 -> Int2 -> Grid b -> Maybe (Grid b)
mapCellAt2 func p1 p2 grid =
    Maybe.map2 func (get p1 grid) (get p2 grid)
        |> Maybe.andThen identity
        |> Maybe.map (\( c1, c2 ) -> grid |> insert p1 c1 |> insert p2 c2)


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
