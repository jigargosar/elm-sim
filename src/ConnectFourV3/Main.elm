module ConnectFourV3.Main exposing (main)

import ConnectFourV3.Grid as Grid exposing (Grid)
import ConnectFourV3.GridTransform as GridTransform exposing (GridTransform)
import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)
import PointFree exposing (flip)
import Set exposing (Set)


type alias Position =
    ( Int, Int )


type Coin
    = Red
    | Blue


type GameOver
    = WinningPositions (Set Position)
    | Draw


type alias Mem =
    { board : Dict Position Coin
    , state : Maybe GameOver
    , coin : Coin
    , grid : Grid Coin
    }


initialMemory : Mem
initialMemory =
    { board = Dict.empty
    , coin = Blue
    , state = Nothing
    , grid = Grid.empty { columns = 7, rows = 6 }
    }


flipCoin : Coin -> Coin
flipCoin coin =
    case coin of
        Red ->
            Blue

        Blue ->
            Red


columnToInsertPosition : Int -> Grid Coin -> Maybe Position
columnToInsertPosition column grid =
    let
        columnLength =
            Dict.filter (\( x, _ ) _ -> x == column) (Grid.toDict grid)
                |> Dict.size

        { columns, rows } =
            Grid.dimension grid
    in
    if column < 0 || column >= columns || (columnLength >= rows) then
        Nothing

    else
        Just ( column, columnLength )


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse, screen } mem =
    let
        gridDimension =
            Grid.dimension mem.grid

        cellSize =
            computeCellSize screen (Grid.dimension mem.grid)

        gt =
            GridTransform.init cellSize gridDimension
    in
    case mem.state of
        Nothing ->
            if mouse.click then
                let
                    column =
                        GridTransform.fromScreenX mouse.x gt
                in
                columnToInsertPosition column mem.grid
                    |> Maybe.map
                        (\position ->
                            let
                                board =
                                    Dict.insert position mem.coin mem.board

                                ( coin, state ) =
                                    computeGameOverState position mem.coin gridDimension board
                            in
                            { mem
                                | board = board
                                , coin = coin
                                , state = state
                            }
                        )
                    |> Maybe.withDefault mem

            else
                mem

        Just _ ->
            mem


computeGameOverState : Position -> Coin -> GridDimension -> Dict Position Coin -> ( Coin, Maybe GameOver )
computeGameOverState position coin { columns, rows } board =
    if Dict.size board >= columns * rows then
        ( coin, Just Draw )

    else
        case getWinningPositions position board of
            Just positionSet ->
                ( coin, Just (WinningPositions positionSet) )

            Nothing ->
                ( flipCoin coin, Nothing )


getWinningPositions : Position -> Dict Position Coin -> Maybe (Set Position)
getWinningPositions position board =
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( 1, -1 ) ]
        |> List.map (getConnectedPositionSetInOpposingDirections position board >> Set.insert position)
        |> List.Extra.find (\positionSet -> Set.size positionSet == 4)


getConnectedPositionSetInOpposingDirections : Position -> Dict Position Coin -> ( Int, Int ) -> Set Position
getConnectedPositionSetInOpposingDirections startPosition board ( dx, dy ) =
    connectedPositionsInDirection ( dx, dy ) startPosition board
        |> Set.fromList
        |> Set.union (connectedPositionsInDirection ( -dx, -dy ) startPosition board |> Set.fromList)


connectedPositionsInDirection : ( Int, Int ) -> Position -> Dict Position Coin -> List Position
connectedPositionsInDirection ( dx, dy ) startPosition board =
    case Dict.get startPosition board of
        Just coin ->
            iterate
                (\( x, y ) ->
                    let
                        newPosition =
                            ( x + dx, y + dy )
                    in
                    if Dict.get newPosition board == Just coin then
                        Just newPosition

                    else
                        Nothing
                )
                startPosition
                []
                |> List.reverse
                |> List.take 3

        Nothing ->
            []


consWhile : (seed -> Maybe ( seed, a )) -> seed -> List a -> List a
consWhile func seed0 aList =
    case func seed0 of
        Just ( seed1, a ) ->
            consWhile func seed1 (a :: aList)

        Nothing ->
            aList


iterate : (a -> Maybe a) -> a -> List a -> List a
iterate func a0 list =
    case func a0 of
        Just a1 ->
            iterate func a1 (a1 :: list)

        Nothing ->
            list


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


computeCellSize : Screen -> GridDimension -> Float
computeCellSize { width, height } { columns, rows } =
    let
        maxCellWidth =
            width * 0.8 / toFloat columns

        maxCellHeight =
            height * 0.8 / toFloat rows
    in
    min maxCellWidth maxCellHeight


viewMemory : Computer -> Mem -> List Shape
viewMemory computer mem =
    let
        gridDimension =
            Grid.dimension mem.grid

        cellSize =
            computeCellSize computer.screen gridDimension

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
toCellList { mouse } gt ({ board } as mem) =
    let
        { rows, columns } =
            Grid.dimension mem.grid

        clampedMouseColumn =
            GridTransform.fromScreenX mouse.x gt
                |> clamp 0 (columns - 1)

        insertIndicatorCoin : Dict Position Cell -> Dict Position Cell
        insertIndicatorCoin =
            case
                columnToInsertPosition clampedMouseColumn mem.grid
            of
                Just pos ->
                    Dict.insert pos (WithCoin True mem.coin)

                Nothing ->
                    identity

        updateWinningPositions : Set Position -> Dict Position Cell -> Dict Position Cell
        updateWinningPositions =
            Set.foldl
                (\pos ->
                    Dict.update pos
                        (Maybe.map
                            (\cell ->
                                case cell of
                                    WithCoin _ coin ->
                                        WithCoin True coin

                                    _ ->
                                        cell
                            )
                        )
                )
                |> flip

        coinBoard : Dict Position Cell
        coinBoard =
            Dict.map (\_ -> WithCoin False) board
                |> (case mem.state of
                        Nothing ->
                            insertIndicatorCoin

                        Just (WinningPositions winningPositionSet) ->
                            updateWinningPositions winningPositionSet

                        Just Draw ->
                            identity
                   )

        emptyBoard : Dict Position Cell
        emptyBoard =
            dimensionToPositioins (Grid.dimension mem.grid)
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
