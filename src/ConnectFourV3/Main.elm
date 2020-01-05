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
    { state : Maybe GameOver
    , coin : Coin
    , grid : Grid Coin
    }


initialMemory : Mem
initialMemory =
    { coin = Blue
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
            Grid.dimensions grid
    in
    if column < 0 || column >= columns || (columnLength >= rows) then
        Nothing

    else
        Just ( column, columnLength )



-- UPDATE


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse, screen } mem =
    let
        gridDimension =
            Grid.dimensions mem.grid

        cellSize =
            computeCellSize screen (Grid.dimensions mem.grid)

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
                            case Grid.insert position mem.coin mem.grid of
                                Ok grid ->
                                    let
                                        ( coin, state ) =
                                            computeGameOverState position mem.coin grid
                                    in
                                    { mem
                                        | grid = grid
                                        , coin = coin
                                        , state = state
                                    }

                                Err _ ->
                                    mem
                        )
                    |> Maybe.withDefault mem

            else
                mem

        Just _ ->
            mem


computeCellSize : Screen -> Grid.Dimensions -> Float
computeCellSize { width, height } { columns, rows } =
    let
        maxCellWidth =
            width * 0.8 / toFloat columns

        maxCellHeight =
            height * 0.8 / toFloat rows
    in
    min maxCellWidth maxCellHeight


computeGameOverState : Position -> Coin -> Grid Coin -> ( Coin, Maybe GameOver )
computeGameOverState startPosition coin grid =
    if Grid.isFull grid then
        ( coin, Just Draw )

    else
        case computeWinningPositionSet startPosition coin grid of
            Just positionSet ->
                ( coin, Just (WinningPositions positionSet) )

            Nothing ->
                ( flipCoin coin, Nothing )


computeWinningPositionSet : Position -> Coin -> Grid Coin -> Maybe (Set Position)
computeWinningPositionSet startPosition coin grid =
    let
        validatePosition : Grid.Position -> Maybe Grid.Position
        validatePosition position =
            if Grid.get position grid == Ok (Just coin) then
                Just position

            else
                Nothing

        getPositionsInDirection ( dx, dy ) =
            collectWhileUpto 3
                (\( x, y ) -> validatePosition ( x + dx, y + dy ))
                startPosition
                |> Set.fromList

        getPositionSetInOpposingDirections ( dx, dy ) =
            getPositionsInDirection ( dx, dy )
                |> Set.union (getPositionsInDirection ( -dx, -dy ))
                |> Set.insert startPosition
    in
    [ ( 1, 0 ), ( 0, 1 ), ( -1, 1 ), ( 1, -1 ) ]
        |> List.map getPositionSetInOpposingDirections
        |> List.Extra.find (\positionSet -> Set.size positionSet == 4)


collectWhileUpto : Int -> (a -> Maybe a) -> a -> List a
collectWhileUpto maxCount nextSeedFunc seed =
    collectWhileUptoHelp maxCount nextSeedFunc seed []
        |> List.reverse


collectWhileUptoHelp : Int -> (a -> Maybe a) -> a -> List a -> List a
collectWhileUptoHelp maxCount nextSeedFunc seed accR =
    if maxCount <= 0 then
        accR

    else
        case nextSeedFunc seed of
            Just nextSeed ->
                collectWhileUptoHelp (maxCount - 1) nextSeedFunc nextSeed (nextSeed :: accR)

            Nothing ->
                accR



-- VIEW


viewMemory : Computer -> Mem -> List Shape
viewMemory computer mem =
    let
        gridDimension =
            Grid.dimensions mem.grid

        cellSize =
            computeCellSize computer.screen gridDimension

        gt =
            GridTransform.init cellSize gridDimension
    in
    [ viewBoard computer gt (toCellList computer gt mem)
    ]


type Cell
    = Empty
    | WithCoin Bool Coin


ignoreError : (b -> Result x b) -> b -> b
ignoreError func val =
    func val |> Result.withDefault val


toCellList : Computer -> GridTransform -> Mem -> List ( Position, Cell )
toCellList { mouse } gt mem =
    let
        { rows, columns } =
            Grid.dimensions mem.grid

        clampedMouseColumn =
            GridTransform.fromScreenX mouse.x gt
                |> clamp 0 (columns - 1)

        insertIndicatorCoin : Grid Cell -> Grid Cell
        insertIndicatorCoin =
            case
                columnToInsertPosition clampedMouseColumn mem.grid
            of
                Just pos ->
                    Grid.insert pos (WithCoin True mem.coin)
                        |> ignoreError

                Nothing ->
                    identity

        updateWinningPositions : Set Position -> Grid Cell -> Grid Cell
        updateWinningPositions =
            Set.foldl
                (\pos ->
                    Grid.update pos
                        (Maybe.map
                            (\cell ->
                                case cell of
                                    WithCoin _ coin ->
                                        WithCoin True coin

                                    _ ->
                                        cell
                            )
                        )
                        |> ignoreError
                )
                |> flip

        coinBoard : Dict Position Cell
        coinBoard =
            Grid.map (\_ -> WithCoin False) mem.grid
                |> (case mem.state of
                        Nothing ->
                            insertIndicatorCoin

                        Just (WinningPositions winningPositionSet) ->
                            updateWinningPositions winningPositionSet

                        Just Draw ->
                            identity
                   )
                |> Grid.toDict

        emptyBoard : Dict Position Cell
        emptyBoard =
            dimensionToPositioins (Grid.dimensions mem.grid)
                |> List.map (\pos -> ( pos, Empty ))
                |> Dict.fromList
    in
    Dict.union coinBoard emptyBoard
        |> Dict.toList


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


dimensionToPositioins : Grid.Dimensions -> List Position
dimensionToPositioins { columns, rows } =
    List.range 0 (columns - 1)
        |> List.concatMap
            (\column ->
                List.range 0 (rows - 1)
                    |> List.map (\row -> ( column, row ))
            )


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



-- Main


main =
    game viewMemory updateMemory initialMemory
