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


cellAt : Int -> Int -> Grid -> Cell
cellAt x y grid =
    Dict.get ( x, y ) grid.cells |> Maybe.withDefault Empty


setCellAt : Int -> Int -> Cell -> Grid -> Grid
setCellAt x y cell grid =
    if cell == Empty then
        { grid | cells = Dict.remove ( x, y ) grid.cells }

    else
        { grid | cells = Dict.insert ( x, y ) cell grid.cells }


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


viewCellAt : Grid -> ( Int, Int ) -> Shape
viewCellAt grid cord =
    let
        r =
            10

        gr =
            r * 2 + 1

        ( x, y ) =
            toViewCord gr cord
    in
    circle blue r
        |> move x y


viewGrid : Grid -> Shape
viewGrid grid =
    List.map (viewCellAt grid) grid.cords
        |> group


main =
    picture [ viewGrid (initGrid 10 10) ]
