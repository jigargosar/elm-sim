module ConnectFourV3.GridTransform exposing
    ( GridTransform
    , cellSize
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
init cs { columns, rows } =
    GridTransform cs
        (-(toFloat columns * cs) / 2 + cs / 2)
        (-(toFloat rows * cs) / 2 + cs / 2)


toScreenX : Int -> GridTransform -> Float
toScreenX column (GridTransform cs dx _) =
    (toFloat column * cs) + dx


toScreenY : Int -> GridTransform -> Float
toScreenY row (GridTransform cs _ dy) =
    (toFloat row * cs) + dy


fromScreenX : Float -> GridTransform -> Int
fromScreenX x (GridTransform cs dx _) =
    (x - dx) / cs |> round


fromScreenY : Float -> GridTransform -> Int
fromScreenY y (GridTransform cs _ dy) =
    (y - dy) / cs |> round


toScreen : ( Int, Int ) -> GridTransform -> ( Float, Float )
toScreen ( column, row ) model =
    ( toScreenX column model, toScreenY row model )


fromScreen : ( Float, Float ) -> GridTransform -> ( Int, Int )
fromScreen ( x, y ) model =
    ( fromScreenX x model, fromScreenY y model )


cellSize : GridTransform -> Float
cellSize (GridTransform cs _ _) =
    cs
