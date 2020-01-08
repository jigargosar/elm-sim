module ConnectFourV3Kata1.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (mapEach, mulBy)



-- MODELS


type alias Pos =
    ( Int, Int )


type Coin
    = Blue
    | Red



-- GRID DIMENSION


type alias GDim =
    { width : Int, height : Int }


centerColumn : GDim -> Int
centerColumn { width } =
    width // 2


gridPositions : GDim -> List Pos
gridPositions { width, height } =
    List.range 0 ((width * height) - 1) |> List.map (\i -> ( i // width, modBy height i ))



-- MEM


type alias Mem =
    { gDim : GDim
    , grid : Dict Pos Coin
    , coin : Coin
    , selectedColumn : Int
    }


init : Mem
init =
    let
        dim =
            { width = 7, height = 6 }
    in
    { gDim = dim
    , grid = Dict.empty
    , coin = Blue
    , selectedColumn = centerColumn dim
    }



-- UPDATE


update : Computer -> Mem -> Mem
update computer mem =
    mem



-- VIEW


type alias Config =
    { cellSize : Float
    , width : Float
    , height : Float
    , dx : Float
    , dy : Float
    }


toConfig : Computer -> Mem -> Config
toConfig computer mem =
    let
        cellSize =
            min (computer.screen.width * 0.7 / toFloat mem.gDim.width)
                (computer.screen.height * 0.7 / toFloat mem.gDim.height)

        ( widthPx, heightPx ) =
            ( toFloat mem.gDim.width * cellSize, toFloat mem.gDim.height * cellSize )
    in
    { cellSize = cellSize
    , width = widthPx
    , height = heightPx
    , dx = (widthPx - cellSize) / 2
    , dy = (heightPx - cellSize) / 2
    }


view : Computer -> Mem -> List Shape
view computer mem =
    let
        c =
            toConfig computer mem
    in
    [ rectangle black c.width c.height
    , gridPositions mem.gDim
        |> List.map
            (\pos ->
                cellBgShape c.cellSize
                    |> moveCellShape c pos
            )
        |> group
    , viewTopIndicator computer.time c mem
    ]


viewTopIndicator : Time -> Config -> Mem -> Shape
viewTopIndicator time c mem =
    cellCoinShape c.cellSize mem.coin
        |> blink time
        |> moveCellShape c ( mem.selectedColumn, mem.gDim.height )


moveCellShape : { a | cellSize : Float, dx : Float, dy : Float } -> Pos -> Shape -> Shape
moveCellShape { cellSize, dx, dy } ( gx, gy ) =
    let
        ( x, y ) =
            ( toFloat gx * cellSize - dx, toFloat gy * cellSize - dy )
    in
    move x y


cellBgShape : Float -> Shape
cellBgShape cellSize =
    circle white (cellSize * 0.5 * 0.9)


cellCoinShape : Float -> Coin -> Shape
cellCoinShape cellSize coin =
    circle
        (case coin of
            Blue ->
                blue

            Red ->
                red
        )
        (cellSize * 0.5 * 0.75)


blink : Time -> Shape -> Shape
blink =
    zigzag 0.5 1 1 >> fade



-- MAIN


main =
    game view update init
