module ConnectFourV3.Main exposing (main)

import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Playground exposing (..)


type alias Position =
    ( Int, Int )


type Coin
    = Red
    | Blue


columns =
    7


rows =
    6


type GameOver
    = WinningPositions (List Position)
    | Draw


type alias Mem =
    { board : Dict Position Coin
    , state : Maybe GameOver
    , coin : Coin
    }


initialMemory : Mem
initialMemory =
    { board = Dict.empty, coin = Blue, state = Nothing }


flipCoin : Coin -> Coin
flipCoin coin =
    case coin of
        Red ->
            Blue

        Blue ->
            Red


insertCoin : Int -> Mem -> Mem
insertCoin column mem =
    if columns < 0 || column >= columns then
        mem

    else
        case mem.state of
            Nothing ->
                let
                    columnLength =
                        Dict.filter (\( x, _ ) _ -> x == column) mem.board
                            |> Dict.size
                in
                if columnLength >= rows then
                    mem

                else
                    { mem
                        | board = Dict.insert ( column, columnLength ) mem.coin mem.board
                        , coin = flipCoin mem.coin
                    }

            Just _ ->
                mem


updateMemory : Computer -> Mem -> Mem
updateMemory _ mem =
    mem


type alias BoardView =
    { cellRadius : Float
    , cellSize : Float
    , width : Float
    , height : Float
    , dx : Float
    , dy : Float
    }


toBoardView : Float -> BoardView
toBoardView cellSize =
    let
        width =
            toFloat columns * cellSize

        height =
            toFloat rows * cellSize

        cellRadius =
            cellSize / 2
    in
    { cellRadius = cellRadius
    , cellSize = cellSize
    , width = width
    , height = height
    , dx = -width / 2 + cellRadius
    , dy = -height / 2 + cellRadius
    }


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { board } =
    let
        { width, height, cellRadius, cellSize, dx, dy } =
            toBoardView 50

        allBoardPositions : List ( Int, Int )
        allBoardPositions =
            List.range 0 (columns - 1)
                |> List.concatMap (\x -> List.range 0 (rows - 1) |> List.map (Tuple.pair x))

        viewCellAt ( x, y ) =
            group
                [ circle white (cellRadius * 0.8)
                , case Dict.get ( x, y ) board of
                    Just coin ->
                        circle (coinToColor coin) (cellRadius * 0.8)

                    Nothing ->
                        group []
                ]
                |> move (toFloat x * cellSize + dx) (toFloat y * cellSize + dy)
    in
    [ group
        [ rectangle black width height
        , List.map viewCellAt allBoardPositions
            |> group
        ]
    ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
