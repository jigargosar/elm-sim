module ConnectFourV3Kata1.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (mapEach, mulBy)


type alias Pos =
    ( Int, Int )


type Coin
    = Blue
    | Red


type alias Mem =
    { width : Int
    , height : Int
    , grid : Dict Pos Coin
    }


init : Mem
init =
    Mem 7 6 Dict.empty


update : Computer -> Mem -> Mem
update computer mem =
    mem


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
            min (computer.screen.width * 0.7 / toFloat mem.width)
                (computer.screen.height * 0.7 / toFloat mem.height)

        ( widthPx, heightPx ) =
            ( toFloat mem.width * cellSize, toFloat mem.height * cellSize )
    in
    { cellSize = cellSize
    , width = widthPx
    , height = heightPx
    , dx = (widthPx - cellSize) / 2
    , dy = (heightPx - cellSize) / 2
    }


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


view : Computer -> Mem -> List Shape
view computer mem =
    let
        cfg =
            toConfig computer mem

        { width, height, cellSize, dx, dy } =
            cfg
    in
    [ rectangle black width height
    , List.range 0 ((mem.width * mem.height) - 1)
        |> List.map
            (\i ->
                let
                    pos =
                        ( i // mem.width, modBy mem.height i )
                in
                cellBgShape cellSize
                    |> moveCellShape cfg pos
            )
        |> group
    ]


main =
    game view update init
