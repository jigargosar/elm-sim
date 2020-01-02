module ConnectFourV3.GridTransform exposing
    ( GridTransform
    , fromScreen
    , fromScreenX
    , fromScreenY
    , init
    , toScreen
    , toScreenX
    , toScreenY
    )


type GridTransform
    = GridTransform Float Float Float


init : Float -> { a | columns : Int, rows : Int } -> GridTransform
init cellSize { columns, rows } =
    GridTransform cellSize
        (-(toFloat columns * cellSize) / 2 + cellSize / 2)
        (-(toFloat rows * cellSize) / 2 + cellSize / 2)


toScreenX : Int -> GridTransform -> Float
toScreenX column (GridTransform cellSize dx _) =
    (toFloat column * cellSize) + dx


toScreenY : Int -> GridTransform -> Float
toScreenY row (GridTransform cellSize _ dy) =
    (toFloat row * cellSize) + dy


fromScreenX : Float -> GridTransform -> Int
fromScreenX x (GridTransform cellSize dx _) =
    (x - dx) / cellSize |> round


fromScreenY : Float -> GridTransform -> Int
fromScreenY y (GridTransform cellSize _ dy) =
    (y - dy) / cellSize |> round


toScreen : ( Int, Int ) -> GridTransform -> ( Float, Float )
toScreen ( column, row ) model =
    ( toScreenX column model, toScreenY row model )


fromScreen : ( Float, Float ) -> GridTransform -> ( Int, Int )
fromScreen ( x, y ) model =
    ( fromScreenX x model, fromScreenY y model )
