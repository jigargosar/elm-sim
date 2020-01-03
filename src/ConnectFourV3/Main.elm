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
updateMemory { mouse, screen } mem =
    let
        gridDimension =
            getGridDimension mem

        cellSize =
            computeCellSize screen (getGridDimension mem)

        gt =
            GridTransform.init cellSize gridDimension
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


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


computeCellSize : Screen -> { a | columns : Int, rows : Int } -> Float
computeCellSize { width, height } { columns, rows } =
    let
        maxCellWidth =
            width * 0.8 / toFloat columns

        maxCellHeight =
            height * 0.8 / toFloat rows
    in
    min maxCellWidth maxCellHeight


getGridDimension : Mem -> GridDimension
getGridDimension { columns, rows } =
    { columns = columns, rows = rows }


viewMemory : Computer -> Mem -> List Shape
viewMemory computer mem =
    let
        gridDimension =
            getGridDimension mem

        cellSize =
            computeCellSize computer.screen (getGridDimension mem)

        gt =
            GridTransform.init cellSize gridDimension
    in
    [ viewBoard computer gt (toCellList computer gt mem)
    ]


type alias GridDimension =
    { columns : Int, rows : Int }


dimensionToPositioins : GridDimension -> List Position
dimensionToPositioins { columns, rows } =
    List.range 0 (columns - 1)
        |> List.concatMap
            (\column ->
                List.range 0 (rows - 1)
                    |> List.map (\row -> ( column, row ))
            )


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
            dimensionToPositioins (getGridDimension mem)
                |> List.map (\pos -> ( pos, Empty ))
                |> Dict.fromList
    in
    Dict.union coinBoard emptyBoard
        |> Dict.toList


viewBoard : Computer -> GridTransform -> List ( Position, Cell ) -> Shape
viewBoard { time } gt cellList =
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

        viewCell ( ( column, row ), cell ) =
            toCellShape cell
                |> move (GridTransform.toScreenX column gt) (GridTransform.toScreenY row gt)
    in
    group
        [ rectangle black (GridTransform.width gt) (GridTransform.height gt)
        , List.map viewCell cellList |> group
        ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
