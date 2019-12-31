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

        viewCell ( ( x, y ), cell ) =
            let
                emptyR =
                    cellSize / 2 * 0.8

                coinR =
                    emptyR * 0.8

                coinShape color =
                    circle color coinR

                shape =
                    group
                        [ circle white emptyR
                        , case cell of
                            Nothing ->
                                group []

                            Just coin ->
                                coinShape
                                    (case coin of
                                        Grid.Red ->
                                            red

                                        Grid.Yellow ->
                                            blue
                                    )
                        ]
            in
            shape |> move (toFloat x * cellSize) (toFloat y * cellSize)
    in
    group
        [ rectangle black widthPx heightPx
        , List.map viewCell (Grid.toCellList grid)
            |> group
            |> move (-widthPx / 2 + cellSize / 2) (-heightPx / 2 + cellSize / 2)
        ]
