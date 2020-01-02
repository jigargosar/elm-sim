module ConnectFourV3.Main exposing (main)

import ConnectFourV3.GridTransform as GridTransform exposing (GridTransform)
import Dict exposing (Dict)
import Playground exposing (..)


type alias Position =
    ( Int, Int )


type Coin
    = Red
    | Blue



{-
   type GameOver
       = WinningPositions (List Position)
       | Draw
-}


type alias GameOver =
    ()


type alias Mem =
    { board : Dict Position Coin
    , state : Maybe GameOver
    , coin : Coin
    , rows : Int
    , columns : Int
    }


initialMemory : Mem
initialMemory =
    let
        columns =
            7

        rows =
            6
    in
    { board = Dict.empty
    , coin = Blue
    , state = Nothing
    , columns = columns
    , rows = rows
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
    let
        gt =
            GridTransform.init defaultCellSize mem
    in
    case mem.state of
        Nothing ->
            mouseClickToBoardColumn mouse gt
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


mouseClickToBoardColumn : Mouse -> GridTransform -> Maybe Int
mouseClickToBoardColumn mouse gt =
    if mouse.click then
        GridTransform.fromScreenX mouse.x gt
            |> Just

    else
        Nothing


type alias BoardView =
    { cellSize : Float
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
    let
        gt =
            GridTransform.init defaultCellSize mem
    in
    [ viewBoard computer
        defaultCellSize
        gt
        mem
        (toCellList computer gt mem)
    ]


type Cell
    = Empty
    | WithCoin Bool Coin


toCellList : Computer -> GridTransform -> Mem -> List ( Position, Cell )
toCellList { mouse } gt ({ rows, columns, board } as mem) =
    let
        clampedMouseColumn =
            GridTransform.fromScreenX mouse.x gt
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


viewBoard : Computer -> Float -> GridTransform -> Mem -> List ( Position, Cell ) -> Shape
viewBoard { time } cellSize gt { columns, rows } cellList =
    let
        cellRadius =
            GridTransform.cellSize gt / 2

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
                |> move (GridTransform.toScreenX x gt) (GridTransform.toScreenY y gt)
    in
    group
        [ rectangle black (toFloat columns * cellSize) (toFloat rows * cellSize)
        , List.map viewCell cellList |> group
        ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
