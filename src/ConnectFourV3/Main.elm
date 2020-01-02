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


columnToInsertPosition : Int -> Mem -> Maybe Position
columnToInsertPosition column mem =
    let
        columnLength =
            Dict.filter (\( x, _ ) _ -> x == column) mem.board
                |> Dict.size
    in
    if column < 0 || column >= mem.columns || (columnLength >= mem.rows) then
        Nothing

    else
        Just ( column, columnLength )


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse } mem =
    case mem.state of
        Nothing ->
            mouseClickToBoardColumn mouse mem
                |> Maybe.andThen (\column -> columnToInsertPosition column mem)
                |> Maybe.map
                    (\position ->
                        { mem
                            | board = Dict.insert position mem.coin mem.board
                            , coin = flipCoin mem.coin
                        }
                    )
                |> Maybe.withDefault mem

        Just _ ->
            mem


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
viewMemory computer mem =
    [ viewBoard computer mem (toCellList computer mem) ]


type Cell
    = Empty
    | WithCoin Bool Coin


toCellList : Computer -> Mem -> List ( Position, Cell )
toCellList { mouse } ({ rows, columns, board } as mem) =
    let
        { cellSize, dx } =
            mem.boardView

        clampedMouseColumn =
            ((mouse.x - dx) / cellSize)
                |> round
                |> clamp 0 (columns - 1)

        insertIndicatorCoin : Dict Position Cell -> Dict Position Cell
        insertIndicatorCoin =
            case
                columnToInsertPosition clampedMouseColumn mem
            of
                Just pos ->
                    Dict.insert pos (WithCoin True mem.coin)

                Nothing ->
                    identity

        coinBoard : Dict Position Cell
        coinBoard =
            Dict.map (\_ -> WithCoin False) board
                |> insertIndicatorCoin

        emptyBoard : Dict Position Cell
        emptyBoard =
            List.range 0 (columns - 1)
                |> List.concatMap
                    (\x ->
                        List.range 0 (rows - 1)
                            |> List.map (\y -> ( ( x, y ), Empty ))
                    )
                |> Dict.fromList
    in
    Dict.union coinBoard emptyBoard
        |> Dict.toList


viewBoard : Computer -> Mem -> List ( Position, Cell ) -> Shape
viewBoard { time } { boardView } cellList =
    let
        { width, height, cellSize, cellRadius, dx, dy } =
            boardView

        coinToShape highlight coin =
            circle (coinToColor coin) (cellRadius * 0.7)
                |> (if highlight then
                        fade (wave 0 1 1 time)

                    else
                        fade 1
                   )

        cellBackgroundShape =
            circle white (cellRadius * 0.9)

        toCellShape cell =
            case cell of
                Empty ->
                    cellBackgroundShape

                WithCoin highlight coin ->
                    group
                        [ cellBackgroundShape
                        , coinToShape highlight coin
                        ]

        viewCell ( ( x, y ), cell ) =
            toCellShape cell
                |> move (toFloat x * cellSize + dx) (toFloat y * cellSize + dy)
    in
    group
        [ rectangle black width height
        , List.map viewCell cellList |> group
        ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
