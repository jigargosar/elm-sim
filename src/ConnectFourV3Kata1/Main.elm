module ConnectFourV3Kata1.Main exposing (main)

import ConnectFourV3Kata1.Board as Board exposing (Board)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (inc, whenTrue)



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
    List.range 0 (height - 1)
        |> List.concatMap
            (\y -> List.range 0 (width - 1) |> List.map (\x -> ( x, y )))


validatePos : Pos -> GDim -> Maybe Pos
validatePos pos { width, height } =
    let
        ( gx, gy ) =
            pos

        isOk =
            (gx < 0 || gy < 0 || gx >= width || gy >= height)
                |> not
    in
    if isOk then
        Just pos

    else
        Nothing



-- MEM


type alias Mem =
    { gDim : GDim
    , board : Board
    , grid : Dict Pos Coin
    , coin : Coin
    , selectedColumn : Int
    }


init : Mem
init =
    let
        dim =
            { width = 7, height = 6 }

        board =
            Board.init dim
    in
    { gDim = dim
    , board = [ 0, 0, 1, 1 ] |> List.foldl Board.insert board
    , grid = Dict.empty
    , coin = Blue
    , selectedColumn = centerColumn dim
    }


insertPosition : Mem -> Maybe Pos
insertPosition mem =
    let
        colLength =
            Dict.foldl (\( x, _ ) _ -> whenTrue (x == mem.selectedColumn) inc) 0 mem.grid
    in
    validatePos ( mem.selectedColumn, colLength ) mem.gDim



-- UPDATE


update : Computer -> Mem -> Mem
update computer mem =
    let
        cfg =
            toConfig computer mem
    in
    if computer.mouse.click then
        let
            column =
                (computer.mouse.x + cfg.dx / cfg.cellSize)
                    |> round
                    |> clamp 0 (mem.gDim.width - 1)
        in
        { mem | board = Board.insert column mem.board }

    else
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


flipCoin : Coin -> Coin
flipCoin coin =
    case coin of
        Blue ->
            Red

        Red ->
            Blue


view : Computer -> Mem -> List Shape
view computer mem =
    let
        c =
            toConfig computer mem

        indicatorShape =
            insertCoinIndicatorShape computer.time c.cellSize mem.coin

        coinGrid =
            Board.positions mem.board
                |> List.foldl
                    (\p ( coin, dict ) ->
                        ( flipCoin coin, Dict.insert p coin dict )
                    )
                    ( Blue, Dict.empty )
                |> Tuple.second
    in
    [ rectangle gray computer.screen.width computer.screen.height
    , rectangle black c.width c.height
    , gridPositions mem.gDim
        |> List.indexedMap
            (\idx pos ->
                group
                    [ cellBgShape c.cellSize
                    , case Dict.get pos coinGrid of
                        Just coin ->
                            cellCoinShape c.cellSize coin

                        Nothing ->
                            group []
                    , if Just pos == insertPosition mem then
                        indicatorShape

                      else
                        group []

                    --, words black (Debug.toString pos)
                    , words black (Debug.toString idx)
                    ]
                    |> moveCell c pos
            )
        |> group
    , indicatorShape |> moveTopIndicator c mem
    ]


insertCoinIndicatorShape : Time -> Float -> Coin -> Shape
insertCoinIndicatorShape time cellSize coin =
    cellCoinShape cellSize coin
        |> blink time


moveTopIndicator : Config -> Mem -> Shape -> Shape
moveTopIndicator c mem =
    moveCell c ( mem.selectedColumn, mem.gDim.height )


moveCell : { a | cellSize : Float, dx : Float, dy : Float } -> Pos -> Shape -> Shape
moveCell { cellSize, dx, dy } ( gx, gy ) =
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
