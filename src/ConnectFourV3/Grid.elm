module ConnectFourV3.Grid exposing (Dimension, Grid, Position, dimension, empty, toDict)

import Dict exposing (Dict)


type alias Dimension =
    { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


type Grid a
    = Grid Dimension (Dict Position a)


empty : Dimension -> Grid a
empty dimension_ =
    Grid dimension_ Dict.empty


toDict : Grid a -> Dict Position a
toDict (Grid _ dict) =
    dict


dimension : Grid a -> Dimension
dimension (Grid dimension_ _) =
    dimension_
