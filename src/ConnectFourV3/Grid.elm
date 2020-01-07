module ConnectFourV3.Grid exposing
    ( Error
    , Grid
    , Position
    , empty
    , foldl
    , get
    , map
    , toDict
    , update
    )

import ConnectFourV3.GridDimensions as Dim exposing (GridDimensions)
import Dict exposing (Dict)


type alias Dimensions =
    GridDimensions


type alias Position =
    ( Int, Int )


type Grid a
    = Grid Dimensions (Dict Position a)


empty : Dimensions -> Grid a
empty dim =
    Grid dim Dict.empty


get : Position -> Grid a -> Result Error (Maybe a)
get position (Grid dim dict) =
    if Dim.contains position dim then
        Dict.get position dict |> Ok

    else
        Err OutOfBounds


type Error
    = OutOfBounds


update : Position -> (Maybe a -> Maybe a) -> Grid a -> Result Error (Grid a)
update position func (Grid dim dict) =
    if Dim.contains position dim then
        Dict.update position func dict |> Grid dim |> Ok

    else
        Err OutOfBounds


map : (Position -> Maybe a -> Maybe b) -> Grid a -> Grid b
map func (Grid dim dict) =
    Dim.foldl
        (\p ->
            case func p (Dict.get p dict) of
                Just b ->
                    Dict.insert p b

                Nothing ->
                    Dict.remove p
        )
        Dict.empty
        dim
        |> Grid dim


foldl : (Position -> Maybe a -> b -> b) -> b -> Grid a -> b
foldl func acc (Grid dim dict) =
    Dim.foldl (\p -> func p (Dict.get p dict)) acc dim


toDict : Grid a -> Dict Position a
toDict (Grid _ dict) =
    dict
