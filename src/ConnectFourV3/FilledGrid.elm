module ConnectFourV3.FilledGrid exposing (Grid)

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
