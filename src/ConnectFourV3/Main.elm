module ConnectFourV3.Main exposing (main)

import ConnectFourV3.GridDimensions as Dim exposing (GridDimensions)
import ConnectFourV3.GridPosition exposing (GridPosition)
import ConnectFourV3.GridTransform as GridTransform exposing (GridTransform)
import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)
import PointFree exposing (flip, is)
import Set exposing (Set)


type Grid a
    = Grid (Dict Pos a)


type alias Dim =
    GridDimensions


type alias Pos =
    GridPosition


type Coin
    = Red
    | Blue


type GameOver
    = WinningPositions (Set Pos)
    | Draw


type alias Mem =
    { state : Maybe GameOver
    , coin : Coin
    , dimensions : Dim
    , grid : Grid Coin
    }


initialMemory : Mem
initialMemory =
    let
        dimensions : Dim
        dimensions =
            Dim.fromColumnsRows { columns = 7, rows = 6 }
    in
    { coin = Blue
    , state = Nothing
    , dimensions = dimensions
    , grid = Grid Dict.empty
    }


flipCoin : Coin -> Coin
flipCoin coin =
    case coin of
        Red ->
            Blue

        Blue ->
            Red


columnToInsertPositionIn : Grid v -> Int -> Pos
columnToInsertPositionIn (Grid dict) column =
    let
        columnLength =
            Dict.foldl
                (\( x, _ ) _ ->
                    if x == column then
                        (+) 1

                    else
                        identity
                )
                0
                dict
    in
    ( column, columnLength )


setInGridAt : Pos -> a -> Dim.GridDimensions -> Grid a -> Maybe (Grid a)
setInGridAt position value dim (Grid dict) =
    if Dim.contains position dim then
        Dict.insert position value dict |> Grid |> Just

    else
        Nothing


updateInGridAt : Pos -> (Maybe v -> Maybe v) -> Dim -> Grid v -> Maybe (Grid v)
updateInGridAt position func dim (Grid dict) =
    if Dim.contains position dim then
        Dict.update position func dict |> Grid |> Just

    else
        Nothing



-- UPDATE


computeGridTransform : Screen -> Dim -> GridTransform
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
            computeGridTransform screen mem.dimensions
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
                case setInGridAt position mem.coin mem.dimensions mem.grid of
                    Just grid ->
                        let
                            ( coin, state ) =
                                computeGameOverState position mem.coin mem.dimensions grid
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
            if mouse.click then
                initialMemory

            else
                mem


computeCellSize : Screen -> Dim -> Float
computeCellSize { width, height } dim =
    let
        { columns, rows } =
            Dim.toColoumnsRows dim

        maxCellWidth =
            width * 0.8 / toFloat columns

        maxCellHeight =
            height * 0.8 / toFloat rows
    in
    min maxCellWidth maxCellHeight


computeGameOverState : Pos -> Coin -> Dim -> Grid Coin -> ( Coin, Maybe GameOver )
computeGameOverState startPosition coin dim ((Grid dict) as grid) =
    if Dict.size dict == Dim.size dim then
        ( coin, Just Draw )

    else
        case computeWinningPositionSet startPosition coin dim grid of
            Just positionSet ->
                ( coin, Just (WinningPositions positionSet) )

            Nothing ->
                ( flipCoin coin, Nothing )


computeWinningPositionSet : Pos -> Coin -> Dim -> Grid Coin -> Maybe (Set Pos)
computeWinningPositionSet startPosition coin dim dict =
    let
        validatePosition : Pos -> Maybe Coin -> Maybe Pos
        validatePosition position maybeCoin =
            if maybeCoin == Just coin then
                Just position

            else
                Nothing

        connectedNeighboursList =
            mapNeighboursWhile startPosition validatePosition dim dict
                |> List.map Set.fromList

        cn1 =
            List.take 4 connectedNeighboursList

        cn2 =
            List.drop 4 connectedNeighboursList
    in
    List.map2 Set.union cn1 cn2
        |> List.Extra.find (Set.size >> is 3)
        |> Maybe.map (Set.insert startPosition)


mapNeighboursWhile : Pos -> (Pos -> Maybe a -> Maybe b) -> Dim -> Grid a -> List (List b)
mapNeighboursWhile startPosition func dim (Grid dict) =
    let
        mapWhileWithStep acc position step =
            case Dim.stepPositionBy step dim position of
                Just nextPosition ->
                    case func nextPosition (Dict.get nextPosition dict) of
                        Just nextValue ->
                            mapWhileWithStep (nextValue :: acc)
                                nextPosition
                                step

                        Nothing ->
                            acc

                Nothing ->
                    acc
    in
    List.map (mapWhileWithStep [] startPosition >> List.reverse)
        Dim.neighboursOffset



-- VIEW


viewMemory : Computer -> Mem -> List Shape
viewMemory { mouse, screen, time } mem =
    let
        gt =
            computeGridTransform screen mem.dimensions

        (Grid dict) =
            mem.grid

        cellViewGrid =
            Dict.map (\_ -> CellView False) dict
                |> Grid
                |> updateCellViewGridWithGameState

        updateCellViewGridWithGameState : Grid CellView -> Grid CellView
        updateCellViewGridWithGameState =
            case mem.state of
                Nothing ->
                    insertIndicatorCoinView mouse gt mem.coin mem.dimensions

                Just (WinningPositions positions) ->
                    highlightWinningPositions mem.dimensions positions

                Just Draw ->
                    identity
    in
    [ group
        [ rectangle black (GridTransform.width gt) (GridTransform.height gt)
        , cellViewGridToShape time gt mem.dimensions cellViewGrid
        ]
    ]


type CellView
    = CellView Bool Coin


insertIndicatorCoinView : Mouse -> GridTransform -> Coin -> Dim -> Grid CellView -> Grid CellView
insertIndicatorCoinView mouse gt coin dim grid =
    let
        unclampedColumn =
            GridTransform.fromScreenX mouse.x gt

        position =
            Dim.clampColoumn unclampedColumn dim
                |> columnToInsertPositionIn grid
    in
    setInGridAt position (CellView True coin) dim grid
        |> Maybe.withDefault grid


highlightWinningPositions : Dim -> Set Pos -> Grid CellView -> Grid CellView
highlightWinningPositions dim =
    Set.foldl
        (\pos grid ->
            updateInGridAt pos
                (Maybe.map
                    (\(CellView _ coin) ->
                        CellView True coin
                    )
                )
                dim
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


cellViewGridToShape : Time -> GridTransform -> Dim.GridDimensions -> Grid CellView -> Shape
cellViewGridToShape time gt dim (Grid dict) =
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
    Dim.foldl (\p -> viewCell p (Dict.get p dict)) [] dim |> group



-- Main


main =
    game viewMemory updateMemory initialMemory
