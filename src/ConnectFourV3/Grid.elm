module ConnectFourV3.Grid exposing
    ( Dimensions
    , Grid
    , Position
    , dimensions
    , empty
    , get
    , insert
    , isFull
    , map
    , toDict
    , update
    )

import Dict exposing (Dict)


type alias Dimensions =
    { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


type Grid a
    = Grid Dimensions (Dict Position a)


empty : Dimensions -> Grid a
empty dimensions_ =
    Grid dimensions_ Dict.empty


toDict : Grid a -> Dict Position a
toDict (Grid _ dict) =
    dict


dimensions : Grid a -> Dimensions
dimensions (Grid dimensions_ _) =
    dimensions_


insert : Position -> a -> Grid a -> Result Error (Grid a)
insert position a (Grid dim dict) =
    if isPositionWithinBounds position dim then
        case Dict.get position dict of
            Just _ ->
                Err NotSuccessful

            Nothing ->
                dict
                    |> Dict.insert position a
                    |> Grid dim
                    |> Ok

    else
        Err OutOfBounds


type Error
    = OutOfBounds
    | NotSuccessful


isPositionWithinBounds : Position -> Dimensions -> Bool
isPositionWithinBounds ( x, y ) { columns, rows } =
    (x < 0 || y < 0 || x >= columns || y >= columns)
        |> not


isFull : Grid a -> Bool
isFull (Grid { columns, rows } dict) =
    rows * columns == Dict.size dict


get : Position -> Grid a -> Result Error (Maybe a)
get position (Grid dim dict) =
    if isPositionWithinBounds position dim then
        Dict.get position dict |> Ok

    else
        Err OutOfBounds


map : (Position -> a -> b) -> Grid a -> Grid b
map func (Grid dim dict) =
    Dict.map func dict
        |> Grid dim


update : Position -> (Maybe a -> Maybe a) -> Grid a -> Result Error (Grid a)
update position func (Grid dim dict) =
    if isPositionWithinBounds position dim then
        Dict.update position func dict |> Grid dim |> Ok

    else
        Err OutOfBounds
