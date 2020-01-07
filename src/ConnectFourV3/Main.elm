module ConnectFourV3.Main exposing (main)

import ConnectFourV3.FilledGrid as Grid
import ConnectFourV3.GridTransform as GridTransform exposing (GridTransform)
import List.Extra
import Playground exposing (..)
import PointFree exposing (flip, is)
import Set exposing (Set)


type alias Grid a =
    Grid.FilledGrid a


type alias Position =
    Grid.Position


type alias CoinGrid =
    Grid (Maybe Coin)


type Coin
    = Red
    | Blue


type GameOver
    = WinningPositions (Set Position)
    | Draw


type alias Mem =
    { state : Maybe GameOver
    , coin : Coin
    , grid : CoinGrid
    }


initialMemory : Mem
initialMemory =
    { coin = Blue
    , state = Nothing
    , grid = Grid.init { columns = 7, rows = 6 } (always Nothing)
    }


flipCoin : Coin -> Coin
flipCoin coin =
    case coin of
        Red ->
            Blue

        Blue ->
            Red


columnToInsertPositionIn : Grid v -> Int -> Position
columnToInsertPositionIn grid column =
    let
        columnLength =
            Grid.count (\( x, _ ) _ -> x == column) grid
    in
    ( column, columnLength )



-- UPDATE


computeGridTransform : Screen -> Grid.Dimensions -> GridTransform
computeGridTransform screen gridDimension =
    let
        cellSize =
            computeCellSize screen gridDimension
    in
    GridTransform.init cellSize gridDimension


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse, screen } mem =
    let
        gt =
            computeGridTransform screen (Grid.dimensions mem.grid)
    in
    case mem.state of
        Nothing ->
            if mouse.click then
                let
                    column =
                        GridTransform.fromScreenX mouse.x gt

                    position =
                        columnToInsertPositionIn mem.grid column
                in
                case Grid.update position (Just mem.coin |> always) mem.grid of
                    Just grid ->
                        let
                            ( coin, state ) =
                                computeGameOverState position mem.coin grid
                        in
                        { mem
                            | grid = grid
                            , coin = coin
                            , state = state
                        }

                    Nothing ->
                        mem

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


computeGameOverState : Position -> Coin -> CoinGrid -> ( Coin, Maybe GameOver )
computeGameOverState startPosition coin grid =
    if Grid.count (\_ -> is Nothing) grid == 0 then
        ( coin, Just Draw )

    else
        case computeWinningPositionSet startPosition coin grid of
            Just positionSet ->
                ( coin, Just (WinningPositions positionSet) )

            Nothing ->
                ( flipCoin coin, Nothing )


computeWinningPositionSet : Position -> Coin -> CoinGrid -> Maybe (Set Position)
computeWinningPositionSet startPosition coin grid =
    let
        validatePosition : Position -> Maybe Position
        validatePosition position =
            if Grid.get position grid == Just (Just coin) then
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
viewMemory { mouse, screen, time } mem =
    let
        gt =
            computeGridTransform screen (Grid.dimensions mem.grid)

        cellViewGrid =
            Grid.map (\_ -> Maybe.map (CellView False)) mem.grid
                |> updateCellViewGridWithGameState

        updateCellViewGridWithGameState =
            case mem.state of
                Nothing ->
                    insertIndicatorCoinView mouse gt mem.coin

                Just (WinningPositions positions) ->
                    highlightWinningPositions positions

                Just Draw ->
                    identity
    in
    [ group
        [ rectangle black (GridTransform.width gt) (GridTransform.height gt)
        , cellViewGridToShape time gt cellViewGrid
        ]
    ]


type CellView
    = CellView Bool Coin


type alias CellViewGrid =
    Grid (Maybe CellView)


insertIndicatorCoinView : Mouse -> GridTransform -> Coin -> CellViewGrid -> CellViewGrid
insertIndicatorCoinView mouse gt coin grid =
    let
        { columns } =
            Grid.dimensions grid

        position =
            GridTransform.fromScreenX mouse.x gt
                |> clamp 0 (columns - 1)
                |> columnToInsertPositionIn grid
    in
    Grid.update position (\_ -> CellView True coin |> Just) grid
        |> Maybe.withDefault grid


highlightWinningPositions : Set Position -> CellViewGrid -> CellViewGrid
highlightWinningPositions =
    Set.foldl
        (\pos grid ->
            Grid.update pos
                (Maybe.map
                    (\(CellView _ coin) ->
                        CellView True coin
                    )
                )
                grid
                |> Maybe.withDefault grid
        )
        |> flip


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


cellViewGridToShape time gt =
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
                Just (CellView highlight coin) ->
                    group
                        [ cellBackgroundShape
                        , coinToShape highlight coin
                        ]

                _ ->
                    cellBackgroundShape

        viewCell ( column, row ) cell =
            toCellShape cell
                |> move (GridTransform.toScreenX column gt) (GridTransform.toScreenY row gt)
                |> (::)
    in
    Grid.foldl viewCell [] >> group



-- Main


main =
    game viewMemory updateMemory initialMemory
