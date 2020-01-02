module ConnectFourV3.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
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
    case mem.state of
        Nothing ->
            case columnToInsertPosition column mem of
                Just position ->
                    { mem
                        | board = Dict.insert position mem.coin mem.board
                        , coin = flipCoin mem.coin
                    }

                Nothing ->
                    mem

        Just _ ->
            mem


columnToInsertPosition : Int -> Mem -> Maybe Position
columnToInsertPosition column mem =
    let
        columnLength =
            Dict.filter (\( x, _ ) _ -> x == column) mem.board
                |> Dict.size
    in
    if
        (columns < 0 || column >= columns)
            && (columnLength >= rows)
    then
        Nothing

    else
        Just ( column, columnLength )


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse } mem =
    mouseClickToBoardColumn mouse
        |> Maybe.map (\column -> insertCoin column mem)
        |> Maybe.withDefault mem


mouseClickToBoardColumn : Mouse -> Maybe Int
mouseClickToBoardColumn mouse =
    if mouse.click then
        let
            { cellSize, dx } =
                toBoardView defaultCellSize
        in
        ((mouse.x - dx) / cellSize)
            |> round
            |> Just

    else
        Nothing


screenXToBoardPosition : Float -> Mem -> Maybe Position
screenXToBoardPosition x mem =
    let
        { cellSize, dx } =
            toBoardView defaultCellSize

        column : Int
        column =
            ((x - dx) / cellSize) |> round
    in
    columnToInsertPosition column mem


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


defaultCellSize =
    50


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { board } =
    let
        { width, height, cellRadius, cellSize, dx, dy } =
            toBoardView defaultCellSize

        allBoardPositions : List ( Int, Int )
        allBoardPositions =
            List.range 0 (columns - 1)
                |> List.concatMap (\x -> List.range 0 (rows - 1) |> List.map (Tuple.pair x))

        moveCellShape ( x, y ) shape =
            shape
                |> move (toFloat x * cellSize + dx) (toFloat y * cellSize + dy)

        coinToShape coin =
            circle (coinToColor coin) (cellRadius * 0.7)

        cellBackgroundShape =
            circle white (cellRadius * 0.9)

        viewCellBackgroundAt : Position -> Shape
        viewCellBackgroundAt position =
            cellBackgroundShape |> moveCellShape position

        viewCoinAt pos coin =
            coinToShape coin |> moveCellShape pos
    in
    [ group
        [ rectangle black width height
        , List.map viewCellBackgroundAt allBoardPositions |> group
        , List.map (uncurry viewCoinAt) (Dict.toList board) |> group
        ]
    ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
