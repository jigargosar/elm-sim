module ConnectFourV3.FilledGrid exposing (Grid, count, foldl, get, init, map, update)

import ConnectFourV3.GridDimensions as GridDimensions
import Dict exposing (Dict)


type alias Dimensions =
    { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


type Grid a
    = Grid Dimensions (Dict Position a)


init : Dimensions -> (Position -> a) -> Grid a
init dim func =
    GridDimensions.foldl (\p -> Dict.insert p (func p)) Dict.empty dim
        |> Grid dim


count : (Position -> a -> Bool) -> Grid a -> Int
count func =
    foldl
        (\p a ->
            if func p a then
                (+) 1

            else
                identity
        )
        0


get : Position -> Grid a -> Maybe a
get position (Grid _ dict) =
    Dict.get position dict


update : Position -> (a -> a) -> Grid a -> Maybe (Grid a)
update position func (Grid dim dict) =
    if GridDimensions.contains position dim then
        Dict.update position (Maybe.map func) dict |> Grid dim |> Just

    else
        Nothing


map : (Position -> a -> b) -> Grid a -> Grid b
map func (Grid dim dict) =
    Dict.map func dict |> Grid dim


foldl : (Position -> a -> b -> b) -> b -> Grid a -> b
foldl func acc (Grid _ dict) =
    Dict.foldl func acc dict
