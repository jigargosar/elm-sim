module ConnectFourV3.SparseGrid exposing
    ( Dimensions
    , Error(..)
    , Position
    , SparseGrid
    , dimensions
    , empty
    , foldl
    , get
    , insert
    , isFull
    , map
    , mapAll
    , toDict
    , update
    )

import Dict exposing (Dict)


type alias Dimensions =
    { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


type SparseGrid a
    = Grid Dimensions (Dict Position a)


empty : Dimensions -> SparseGrid a
empty dimensions_ =
    Grid dimensions_ Dict.empty


toDict : SparseGrid a -> Dict Position a
toDict (Grid _ dict) =
    dict


dimensions : SparseGrid a -> Dimensions
dimensions (Grid dimensions_ _) =
    dimensions_


insert : Position -> a -> SparseGrid a -> Result Error (SparseGrid a)
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


isFull : SparseGrid a -> Bool
isFull (Grid { columns, rows } dict) =
    rows * columns == Dict.size dict


get : Position -> SparseGrid a -> Result Error (Maybe a)
get position (Grid dim dict) =
    if isPositionWithinBounds position dim then
        Dict.get position dict |> Ok

    else
        Err OutOfBounds


map : (Position -> a -> b) -> SparseGrid a -> SparseGrid b
map func (Grid dim dict) =
    Dict.map func dict
        |> Grid dim


update : Position -> (Maybe a -> Maybe a) -> SparseGrid a -> Result Error (SparseGrid a)
update position func (Grid dim dict) =
    if isPositionWithinBounds position dim then
        Dict.update position func dict |> Grid dim |> Ok

    else
        Err OutOfBounds


mapAll : (Position -> Maybe a -> Maybe b) -> SparseGrid a -> SparseGrid b
mapAll func ((Grid dim _) as grid) =
    foldl
        (\position maybeA ->
            case func position maybeA of
                Just b ->
                    Dict.insert position b

                Nothing ->
                    Dict.remove position
        )
        Dict.empty
        grid
        |> Grid dim


foldl : (Position -> Maybe a -> b -> b) -> b -> SparseGrid a -> b
foldl func acc0 (Grid { columns, rows } dict) =
    List.range 0 (columns - 1)
        |> List.foldl
            (\column acc1 ->
                List.range 0 (rows - 1)
                    |> List.foldl
                        (\row ->
                            let
                                position =
                                    ( column, row )
                            in
                            func position (Dict.get position dict)
                        )
                        acc1
            )
            acc0
