module ConnectFour exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Playground exposing (..)


type Cell
    = Red
    | Yellow
    | Empty


gridWidth =
    10


gridHeight =
    10


type alias Grid =
    { width : Int, height : Int, cords : List ( Int, Int ), cells : Dict ( Int, Int ) Cell }


initGrid : Int -> Int -> Grid
initGrid w h =
    let
        cords =
            List.range 0 w
                |> List.map (\x -> List.range 0 h |> List.map (\y -> ( x, y )))
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
            black

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
        cellSize =
            50
    in
    List.map (viewCellAt cellSize grid) grid.cords
        |> group
        |> moveDown (toFloat grid.height * cellSize / 2)
        |> moveLeft (toFloat grid.width * cellSize / 2)


main =
    picture [ viewGrid (initGrid 7 7) ]