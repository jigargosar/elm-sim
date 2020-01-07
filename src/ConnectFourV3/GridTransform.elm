module ConnectFourV3.GridTransform exposing
    ( GridTransform
    , cellSize
    , fromScreen
    , fromScreenX
    , fromScreenY
    , height
    , init
    , toScreen
    , toScreenX
    , toScreenY
    , width
    )

import ConnectFourV3.GridDimensions as GridDimensions exposing (GridDimensions)


type GridTransform
    = GridTransform Record


unwrap : GridTransform -> Record
unwrap (GridTransform r) =
    r


type alias Record =
    { cs : Float
    , dx : Float
    , dy : Float
    , rows : Int
    , columns : Int
    , width : Float
    , height : Float
    }


init : Float -> GridDimensions -> GridTransform
init cs dim =
    let
        { columns, rows } =
            GridDimensions.toColoumnsRows dim
    in
    { cs = cs
    , dx =
        -(toFloat columns * cs) / 2 + cs / 2
    , dy = -(toFloat rows * cs) / 2 + cs / 2
    , rows = rows
    , columns = columns
    , width = toFloat columns * cs
    , height = toFloat rows * cs
    }
        |> GridTransform


toScreenX : Int -> GridTransform -> Float
toScreenX column (GridTransform { cs, dx }) =
    (toFloat column * cs) + dx


toScreenY : Int -> GridTransform -> Float
toScreenY row (GridTransform { cs, dy }) =
    (toFloat row * cs) + dy


fromScreenX : Float -> GridTransform -> Int
fromScreenX x (GridTransform { cs, dx }) =
    (x - dx) / cs |> round


fromScreenY : Float -> GridTransform -> Int
fromScreenY y (GridTransform { cs, dy }) =
    (y - dy) / cs |> round


toScreen : ( Int, Int ) -> GridTransform -> ( Float, Float )
toScreen ( column, row ) model =
    ( toScreenX column model, toScreenY row model )


fromScreen : ( Float, Float ) -> GridTransform -> ( Int, Int )
fromScreen ( x, y ) model =
    ( fromScreenX x model, fromScreenY y model )


cellSize : GridTransform -> Float
cellSize (GridTransform { cs }) =
    cs


width : GridTransform -> Float
width =
    unwrap >> .width


height : GridTransform -> Float
height =
    unwrap >> .height
