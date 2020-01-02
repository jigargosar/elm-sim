module ConnectFourV3.Main exposing (main)

import Dict exposing (Dict)
import Playground exposing (..)


type alias Position =
    ( Int, Int )


type Coin
    = Red
    | Blue


type GameOver
    = WinningPositions (List Position)
    | Draw


type alias Mem =
    { board : Dict Position Coin
    , state : Maybe GameOver
    , coin : Coin
    , rows : Int
    , columns : Int
    , boardView : BoardView
    }


initialMemory : Mem
initialMemory =
    let
        columns =
            7

        rows =
            6

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
    in
    { board = Dict.empty
    , coin = Blue
    , state = Nothing
    , columns = columns
    , rows = rows
    , boardView = toBoardView defaultCellSize
    }


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
        (mem.columns < 0 || column >= mem.columns)
            && (columnLength >= mem.rows)
    then
        Nothing

    else
        Just ( column, columnLength )


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse } mem =
    mouseClickToBoardColumn mouse mem
        |> Maybe.map (\column -> insertCoin column mem)
        |> Maybe.withDefault mem


mouseClickToBoardColumn : Mouse -> Mem -> Maybe Int
mouseClickToBoardColumn mouse mem =
    if mouse.click then
        let
            { cellSize, dx } =
                mem.boardView
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
            mem.boardView

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
viewMemory { mouse } ({ board } as mem) =
    viewBoard mem


viewBoard { boardView, board, rows, columns } =
    let
        { width, height, cellSize, cellRadius, dx, dy } =
            boardView

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

        viewBoardCoin : ( Position, Coin ) -> Shape
        viewBoardCoin ( pos, coin ) =
            coinToShape coin |> moveCellShape pos
    in
    [ group
        [ rectangle black width height
        , List.map viewCellBackgroundAt allBoardPositions |> group
        , List.map viewBoardCoin (Dict.toList board) |> group
        ]
    ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
