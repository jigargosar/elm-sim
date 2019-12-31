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
    in
    group
        [ rectangle black widthPx heightPx
        , List.map (cellToShape cellSize) (Grid.toCellList grid)
            |> group
            |> move (-widthPx / 2 + cellSize / 2) (-heightPx / 2 + cellSize / 2)
        ]


cellToShape : Float -> ( ( Int, Int ), Maybe Grid.Coin ) -> Shape
cellToShape cellSize ( ( x, y ), cell ) =
    let
        cellR =
            cellSize / 2

        bgR =
            cellR * 0.8

        cellBackgroundShape =
            circle white bgR

        coinR =
            bgR * 0.8

        coinShapeFromColor color =
            circle color coinR

        maybeCoinColor : Maybe Color
        maybeCoinColor =
            cell
                |> Maybe.map
                    (\coin ->
                        case coin of
                            Grid.Red ->
                                red

                            Grid.Yellow ->
                                blue
                    )

        shape =
            group
                [ cellBackgroundShape
                , maybeCoinColor
                    |> Maybe.map coinShapeFromColor
                    |> Maybe.withDefault (group [])
                ]
    in
    shape |> move (toFloat x * cellSize) (toFloat y * cellSize)
