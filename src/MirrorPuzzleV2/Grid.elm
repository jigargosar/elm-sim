module MirrorPuzzleV2.Grid exposing
    ( Grid
    , Pos
    , filled
    , foldl
    , get
    , insert
    , isValid
    , map
    , toDict
    , toList
    , values
    , viewDimensions
    )

import Dict exposing (Dict)
import PointFree exposing (mapEach, mulBy, when)


type alias Pos =
    ( Int, Int )


type Grid a
    = Grid Int Int (Dict Pos a)


filled : Int -> Int -> a -> Grid a
filled w h a =
    foldWH (\pos -> Dict.insert pos a) Dict.empty w h |> Grid w h


get : Pos -> Grid a -> Maybe a
get pos =
    toDict >> Dict.get pos


insert : Pos -> b -> Grid b -> Grid b
insert pos a =
    when (isValid pos)
        (mapDict (Dict.insert pos a))


toDict : Grid a -> Dict Pos a
toDict (Grid _ _ dict) =
    dict


viewDimensions : Float -> Grid a -> ( Float, Float )
viewDimensions cellSize (Grid w h _) =
    ( w, h ) |> mapEach (toFloat >> mulBy cellSize)


isValid : Pos -> Grid a -> Bool
isValid ( x, y ) (Grid w h _) =
    isValidIdx w x && isValidIdx h y


map : (Pos -> a -> b) -> Grid a -> Grid b
map func =
    mapDict (Dict.map func)


values : Grid a -> List a
values =
    toDict >> Dict.values


foldl : (Pos -> a -> b -> b) -> b -> Grid a -> b
foldl func acc =
    toDict >> Dict.foldl func acc


toList : Grid a -> List ( Pos, a )
toList =
    toDict >> Dict.toList


isValidIdx : number -> number -> Bool
isValidIdx len idx =
    idx >= 0 && idx < len


mapDict : (Dict Pos a -> Dict Pos b) -> Grid a -> Grid b
mapDict func (Grid w h dict) =
    func dict |> Grid w h


foldWH : (Pos -> b -> b) -> b -> Int -> Int -> b
foldWH func acc0 w h =
    List.range 0 (h - 1)
        |> List.foldl
            (\y acc1 ->
                List.range 0 (w - 1)
                    |> List.foldl (\x -> func ( x, y )) acc1
            )
            acc0
