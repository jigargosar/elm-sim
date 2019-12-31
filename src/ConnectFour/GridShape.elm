module ConnectFour.GridShape exposing (..)

import ConnectFour.Grid as Grid exposing (Grid)
import Playground exposing (..)
import PointFree exposing (mapEach)


withCellSize : Float -> Grid -> Shape
withCellSize cellSize grid =
    let
        ( widthPx, heightPx ) =
            ( Grid.width grid, Grid.height grid )
                |> mapEach (toFloat >> (*) cellSize)

        positions =
            Grid.allPositions grid

        emptyCellRadius =
            cellSize / 2 * 0.8

        viewEmptyCell ( x, y ) =
            circle white emptyCellRadius
                |> move (toFloat x * cellSize) (toFloat y * cellSize)
    in
    group
        [ rectangle black widthPx heightPx
        , List.map viewEmptyCell positions
            |> group
            |> move (-widthPx / 2 + cellSize / 2) (-heightPx / 2 + cellSize / 2)
        ]
