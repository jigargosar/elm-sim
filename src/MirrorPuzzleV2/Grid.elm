module MirrorPuzzleV2.Grid exposing (Grid, Pos, filled, get, insert)

import Dict exposing (Dict)
import PointFree exposing (when)


type alias Pos =
    ( Int, Int )


type Grid a
    = Grid Int Int (Dict Pos a)


filled : Int -> Int -> a -> Grid a
filled w h a =
    foldWH (\pos -> Dict.insert pos a) Dict.empty w h |> Grid w h


get : Pos -> Grid v -> Maybe v
get pos =
    toDict >> Dict.get pos


insert : Pos -> b -> Grid b -> Grid b
insert pos a =
    when (isValid pos)
        (mapDict (Dict.insert pos a))


isValid : Pos -> Grid a -> Bool
isValid ( x, y ) (Grid w h _) =
    isValidIdx w x && isValidIdx h y


isValidIdx : number -> number -> Bool
isValidIdx len idx =
    idx >= 0 && idx < len


mapDict : (Dict Pos a -> Dict Pos b) -> Grid a -> Grid b
mapDict func (Grid w h dict) =
    func dict |> Grid w h


toDict : Grid a -> Dict Pos a
toDict (Grid _ _ dict) =
    dict


foldWH : (Pos -> b -> b) -> b -> Int -> Int -> b
foldWH func acc0 w h =
    List.range 0 (h - 1)
        |> List.foldl
            (\y acc1 ->
                List.range 0 (w - 1)
                    |> List.foldl (\x -> func ( x, y )) acc1
            )
            acc0
