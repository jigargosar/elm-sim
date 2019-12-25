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


update : Computer -> Grid -> Grid
update { mouse } mem =
    let
        gvm =
            toGridViewModel mem

        mouseGridCord =
            screenCordToGridCord gvm ( mouse.x, mouse.y )

        _ =
            if mouse.click then
                Debug.log "mouseGridCord" mouseGridCord

            else
                mouseGridCord
    in
    mem


screenCordToGridCord : GridViewModel -> ( Float, Float ) -> ( Int, Int )
screenCordToGridCord gvm ( x, y ) =
    ( (x - gvm.left) / gvm.cellSize |> round, (y - gvm.bottom) / gvm.cellSize |> round )


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


gridCordToScreenCord : GridViewModel -> ( Int, Int ) -> ( Float, Float )
gridCordToScreenCord gvm ( x, y ) =
    ( gvm.left + toFloat x * gvm.cellSize, gvm.bottom + toFloat y * gvm.cellSize )


viewGridCell : GridViewModel -> Grid -> ( Int, Int ) -> Shape
viewGridCell gvm grid cord =
    let
        cell =
            cellAt cord grid

        ( x, y ) =
            gridCordToScreenCord gvm cord
    in
    circle (cellColor cell) gvm.cellRadius
        |> move x y


viewGrid : Grid -> Shape
viewGrid grid =
    let
        gvm =
            toGridViewModel grid

        off =
            gvm.cellSize
    in
    group
        [ rectangle blue (gvm.width + off) (gvm.height + off)
        , List.map (viewGridCell gvm grid) grid.cords |> group
        ]


type alias GridViewModel =
    { width : Float
    , height : Float
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
    , cellSize : Number
    , cellRadius : Number
    }


toGridViewModel : Grid -> GridViewModel
toGridViewModel grid =
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
