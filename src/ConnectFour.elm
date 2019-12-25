module ConnectFour exposing (..)

import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (flip)


type Cell
    = Red
    | Yellow
    | Empty


type alias Grid =
    { width : Int, height : Int, cords : List ( Int, Int ), cells : Dict ( Int, Int ) Cell }


initGrid : Int -> Int -> Grid
initGrid w h =
    let
        cords =
            List.range 0 (w - 1)
                |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
                |> List.concat
                |> List.sort
                |> Debug.log "cords"
    in
    { width = w, height = h, cords = cords, cells = Dict.empty }


cellAt : ( Int, Int ) -> Grid -> Cell
cellAt cord grid =
    Dict.get cord grid.cells |> Maybe.withDefault Empty


setCellAt : ( Int, Int ) -> Cell -> Grid -> Grid
setCellAt cord cell grid =
    { grid | cells = Dict.insert cord cell grid.cells }


initialMem : Grid
initialMem =
    initGrid 10 18
        |> setCellAt ( 0, 0 ) Yellow
        |> setCellAt ( 0, 1 ) Red
        |> setCellAt ( 0, 2 ) Yellow
        |> setCellAt ( 7, 0 ) Yellow


update _ mem =
    mem


view _ grid =
    [ viewGrid grid ]


toViewCord : Float -> ( Int, Int ) -> ( Float, Float )
toViewCord mul =
    let
        f =
            toFloat >> (*) mul
    in
    Tuple.mapBoth f f


cellColor : Cell -> Color
cellColor cell =
    case cell of
        Empty ->
            white

        Red ->
            red

        Yellow ->
            yellow


viewCell : Number -> Cell -> Shape
viewCell r cell =
    circle (cellColor cell) r


viewCellAt : Float -> Grid -> ( Int, Int ) -> Shape
viewCellAt size grid cord =
    let
        cellR =
            size / 2 - size / 10

        ( x, y ) =
            toViewCord size cord
    in
    viewCell cellR (cellAt cord grid)
        |> move x y


viewGrid : Grid -> Shape
viewGrid grid =
    let
        gs =
            toGridScreen grid

        off =
            gs.cellSize

        cellList =
            List.map (flip cellAt grid) grid.cords

        toScreenCord ( x, y ) =
            ( gs.left + toFloat x * gs.cellSize, gs.bottom + toFloat y * gs.cellSize )

        screenCordList =
            List.map toScreenCord grid.cords

        screenCordCellPairs =
            List.map2 Tuple.pair screenCordList cellList

        viewCell2 ( ( x, y ), cell ) =
            circle (cellColor cell) gs.cellRadius
                |> move x y
    in
    group
        [ rectangle blue (gs.width + off) (gs.height + off)
        , List.map viewCell2 screenCordCellPairs
            |> group

        {- |> moveDown (gs.height / 2)
           |> moveLeft (gs.width / 2)
        -}
        ]


type alias GridScreen =
    { width : Float
    , height : Float
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
    , cellSize : Number
    , cellRadius : Number
    }


toGridScreen : Grid -> GridScreen
toGridScreen grid =
    let
        cellSize =
            50

        width =
            toFloat (grid.width - 1) * cellSize

        height =
            toFloat (grid.height - 1) * cellSize
    in
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    , cellSize = cellSize
    , cellRadius = cellSize / 2 - cellSize / 10
    }


main =
    game view update initialMem
