module ConnectFourV3.FilledGrid exposing
    ( Dimensions
    , FilledGrid
    , Position
    , count
    , dimensions
    , foldl
    , get
    , init
    , map
    , size
    , update
    )

import ConnectFourV3.GridDimensions as GridDimensions
import Dict exposing (Dict)


type alias Dimensions =
    { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


type FilledGrid a
    = Grid Dimensions (Dict Position a)


init : Dimensions -> (Position -> a) -> FilledGrid a
init dim func =
    GridDimensions.foldl (\p -> Dict.insert p (func p)) Dict.empty dim
        |> Grid dim


count : (Position -> a -> Bool) -> FilledGrid a -> Int
count func =
    foldl
        (\p a ->
            if func p a then
                (+) 1

            else
                identity
        )
        0


size : FilledGrid a -> Int
size (Grid dim _) =
    GridDimensions.size dim


get : Position -> FilledGrid a -> Maybe a
get position (Grid _ dict) =
    Dict.get position dict


update : Position -> (a -> a) -> FilledGrid a -> Maybe (FilledGrid a)
update position func (Grid dim dict) =
    Dict.get position dict
        |> Maybe.map (\a -> Dict.insert position (func a) dict |> Grid dim)


map : (Position -> a -> b) -> FilledGrid a -> FilledGrid b
map func (Grid dim dict) =
    Dict.map func dict |> Grid dim


foldl : (Position -> a -> b -> b) -> b -> FilledGrid a -> b
foldl func acc (Grid _ dict) =
    Dict.foldl func acc dict


dimensions : FilledGrid a -> Dimensions
dimensions (Grid dim _) =
    dim
