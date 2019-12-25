module ConnectFour exposing (..)

import Dict exposing (Dict)
import Playground exposing (..)


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


cellColor : Cell -> Color
cellColor cell =
    case cell of
        Empty ->
            white

        Red ->
            red

        Yellow ->
            yellow


gridCordToScreenCord : GridScreen -> ( Int, Int ) -> ( Float, Float )
gridCordToScreenCord gs ( x, y ) =
    ( gs.left + toFloat x * gs.cellSize, gs.bottom + toFloat y * gs.cellSize )


viewGridCell : GridScreen -> Grid -> ( Int, Int ) -> Shape
viewGridCell gs grid cord =
    let
        cell =
            cellAt cord grid

        ( x, y ) =
            gridCordToScreenCord gs cord
    in
    circle (cellColor cell) gs.cellRadius
        |> move x y


viewGrid : Grid -> Shape
viewGrid grid =
    let
        gs =
            toGridScreen grid

        off =
            gs.cellSize
    in
    group
        [ rectangle blue (gs.width + off) (gs.height + off)
        , List.map (viewGridCell gs grid) grid.cords |> group
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
