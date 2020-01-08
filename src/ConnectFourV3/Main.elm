module ConnectFourV3.Main exposing (main)

import ConnectFourV3.GridDimensions as Dim exposing (GridDimensions)
import ConnectFourV3.GridPosition exposing (GridPosition)
import ConnectFourV3.GridTransform as GTransform exposing (GridTransform)
import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)
import PointFree exposing (flip, is, whenTrue)
import Set exposing (Set)



-- Shorthand


type alias Dim =
    GridDimensions


type alias Pos =
    GridPosition



-- Grid


type Grid a
    = Grid Dim (Dict Pos a)


clampAndInsertInColumn : Int -> a -> Grid a -> Maybe ( Pos, Grid a )
clampAndInsertInColumn column_ a (Grid dim dict) =
    let
        column =
            Dim.clampColoumn column_ dim

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

        position =
            ( column, columnLength )
    in
    if Dim.contains position dim then
        Just ( position, Dict.insert position a dict |> Grid dim )

    else
        Nothing


isValidGridColumn : Int -> Grid a -> Bool
isValidGridColumn column (Grid dim _) =
    Dim.containsColumn column dim


insertInColumn : Int -> a -> Grid a -> Result InsertError ( Pos, Grid a )
insertInColumn column a grid =
    if isValidGridColumn column grid then
        case clampAndInsertInColumn column a grid of
            Just ret ->
                Ok ret

            Nothing ->
                Err ColumnFull

    else
        Err InvalidColumn


type InsertError
    = InvalidColumn
    | ColumnFull


updateGridAt : Pos -> (Maybe v -> Maybe v) -> Grid v -> Maybe (Grid v)
updateGridAt position func (Grid dim dict) =
    if Dim.contains position dim then
        Dict.update position func dict |> Grid dim |> Just

    else
        Nothing


updateGridPositions : Set Pos -> (Maybe v -> Maybe v) -> Grid v -> Maybe (Grid v)
updateGridPositions positions func grid =
    Set.foldl (\p -> Maybe.andThen (updateGridAt p func)) (Just grid) positions


gridDimension : Grid a -> Dim
gridDimension (Grid dim _) =
    dim


mapGridDictValues : (a -> b) -> Grid a -> Grid b
mapGridDictValues func (Grid dim dict) =
    Dict.map (always func) dict |> Grid dim


foldlGrid : (Pos -> Maybe a -> b -> b) -> b -> Grid a -> b
foldlGrid func acc (Grid dim dict) =
    Dim.foldl (\p -> func p (Dict.get p dict)) acc dim


isGridFull : Grid a -> Bool
isGridFull (Grid dim dict) =
    Dim.size dim == Dict.size dict


mapNeighboursWhile : Pos -> (Pos -> Maybe a -> Maybe b) -> Grid a -> List (List b)
mapNeighboursWhile startPosition func (Grid dim dict) =
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



-- Model


type Coin
    = Red
    | Blue


type GameOver
    = WinningPositions (Set Pos)
    | Draw


type alias Mem =
    { state : Maybe GameOver
    , coin : Coin
    , grid : Grid Coin
    , selectedColumn : Maybe Int
    }


initialMemory : Mem
initialMemory =
    let
        dim : Dim
        dim =
            Dim.fromColumnsRows { columns = 7, rows = 6 }
    in
    { coin = Blue
    , state = Nothing
    , grid = Grid dim Dict.empty
    , selectedColumn = Nothing
    }


flipCoin : Coin -> Coin
flipCoin coin =
    case coin of
        Red ->
            Blue

        Blue ->
            Red



-- UPDATE


computeGridTransform : Screen -> Dim -> GridTransform
computeGridTransform screen dim =
    let
        cellSize =
            computeCellSize screen dim
    in
    GTransform.init cellSize dim


updateMemory : Computer -> Mem -> Mem
updateMemory { mouse, screen } mem =
    let
        gt =
            computeGridTransform screen dim

        dim =
            gridDimension mem.grid
    in
    case mem.state of
        Nothing ->
            if mouse.click then
                let
                    column =
                        GTransform.fromScreenX mouse.x gt
                in
                if Dim.containsColumn column dim then
                    if selectedColumnToColumn mem.selectedColumn dim == column then
                        case insertInColumn column mem.coin mem.grid of
                            Ok ( position, grid ) ->
                                let
                                    ( coin, state ) =
                                        computeGameOverState position mem.coin grid
                                in
                                { mem
                                    | grid = grid
                                    , coin = coin
                                    , state = state
                                    , selectedColumn = Nothing
                                }

                            Err ColumnFull ->
                                mem

                            Err InvalidColumn ->
                                mem

                    else
                        { mem | selectedColumn = Just column }

                else
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
            width * 0.7 / toFloat columns

        maxCellHeight =
            height * 0.7 / toFloat rows
    in
    min maxCellWidth maxCellHeight


computeGameOverState : Pos -> Coin -> Grid Coin -> ( Coin, Maybe GameOver )
computeGameOverState startPosition coin grid =
    if isGridFull grid then
        ( coin, Just Draw )

    else
        case computeWinningPositionSet startPosition coin grid of
            Just positionSet ->
                ( coin, Just (WinningPositions positionSet) )

            Nothing ->
                ( flipCoin coin, Nothing )


computeWinningPositionSet : Pos -> Coin -> Grid Coin -> Maybe (Set Pos)
computeWinningPositionSet startPosition coin grid =
    let
        validatePosition : Pos -> Maybe Coin -> Maybe Pos
        validatePosition position maybeCoin =
            if maybeCoin == Just coin then
                Just position

            else
                Nothing

        connectedNeighboursList =
            mapNeighboursWhile startPosition validatePosition grid
                |> List.map Set.fromList

        cn1 =
            List.take 4 connectedNeighboursList

        cn2 =
            List.drop 4 connectedNeighboursList
    in
    List.map2 Set.union cn1 cn2
        |> List.Extra.find (Set.size >> is 3)
        |> Maybe.map (Set.insert startPosition)



-- VIEW


viewMemory : Computer -> Mem -> List Shape
viewMemory { mouse, screen, time } mem =
    let
        gt =
            computeGridTransform screen dim

        dim =
            gridDimension mem.grid

        cfg =
            toCellViewConfig time gt

        indicatorColumn =
            selectedColumnToColumn mem.selectedColumn dim

        updateCellViewGridWithGameState : Grid CellView -> Maybe (Grid CellView)
        updateCellViewGridWithGameState =
            case mem.state of
                Nothing ->
                    insertIndicatorCoinView indicatorColumn mem.coin

                Just (WinningPositions positions) ->
                    updateGridPositions positions (Maybe.map highlightCellView)

                Just Draw ->
                    Just
    in
    case
        mapGridDictValues cellViewFromCoin mem.grid
            |> updateCellViewGridWithGameState
    of
        Just cellViewGrid ->
            [ group
                [ rectangle black (GTransform.width gt) (GTransform.height gt)
                , cellViewGridToShape cfg cellViewGrid
                , gameStateToWordsShape mem.coin mem.state
                    |> scale 1.5
                    |> moveY (GTransform.bottom gt)
                    |> moveY -30
                , case mem.state of
                    Nothing ->
                        coinToShape cfg mem.coin
                            |> applyHighlight cfg
                            |> moveToGridPos cfg
                                ( indicatorColumn
                                , Dim.lastRow dim + 1
                                )

                    _ ->
                        group []
                ]
            ]

        Nothing ->
            [ words red "error updating view with gameover state" ]


applyHighlight : CellViewConfig -> Shape -> Shape
applyHighlight { time } =
    fade (wave 0.7 1 1 time)


gameStateToWordsShape : Coin -> Maybe GameOver -> Shape
gameStateToWordsShape coin state =
    let
        ( color, message ) =
            case state of
                Just Draw ->
                    ( black, "Game Draw" )

                Just (WinningPositions _) ->
                    ( coinToColor coin, coinToString coin ++ " Won!" )

                Nothing ->
                    ( coinToColor coin, coinToString coin ++ "'s turn" )
    in
    words color message


coinToString : Coin -> String
coinToString coin =
    case coin of
        Blue ->
            "Blue"

        Red ->
            "Red"


type CellView
    = CellView CellViewRecord


type alias CellViewRecord =
    { highlight : Bool, coin : Coin }


cellViewFromCoin : Coin -> CellView
cellViewFromCoin coin =
    CellView { highlight = False, coin = coin }


highlightCellView : CellView -> CellView
highlightCellView (CellView rec) =
    CellView { rec | highlight = True }


selectedColumnToColumn : Maybe Int -> Dim -> Int
selectedColumnToColumn selectedColumn dim =
    selectedColumn
        |> Maybe.map (flip Dim.clampColoumn dim)
        |> Maybe.withDefault (Dim.centerColumn dim)


insertIndicatorCoinView : Int -> Coin -> Grid CellView -> Maybe (Grid CellView)
insertIndicatorCoinView column coin grid =
    let
        cellView =
            cellViewFromCoin coin
                |> highlightCellView
    in
    clampAndInsertInColumn column cellView grid
        |> Maybe.map Tuple.second
        |> Maybe.withDefault grid
        |> Just


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


type alias CellViewConfig =
    { bgRadius : Float
    , coinRadius : Float
    , time : Time
    , gt : GridTransform
    }


toCellViewConfig : Time -> GridTransform -> CellViewConfig
toCellViewConfig time gt =
    let
        cellRadius : Float
        cellRadius =
            GTransform.cellSize gt / 2
    in
    { bgRadius = cellRadius * 0.9
    , coinRadius = cellRadius * 0.75
    , time = time
    , gt = gt
    }


coinToShape : CellViewConfig -> Coin -> Shape
coinToShape cfg coin =
    circle (coinToColor coin) cfg.coinRadius


cellBackgroundShape : CellViewConfig -> Shape
cellBackgroundShape =
    .bgRadius >> circle white


moveToGridPos : CellViewConfig -> ( Int, Int ) -> Shape -> Shape
moveToGridPos { gt } ( column, row ) =
    move (GTransform.toScreenX column gt) (GTransform.toScreenY row gt)


cellViewGridToShape : CellViewConfig -> Grid CellView -> Shape
cellViewGridToShape cfg grid =
    let
        { gt, coinRadius, bgRadius } =
            cfg

        toCellShape : Maybe CellView -> Shape
        toCellShape cell =
            case cell of
                Just (CellView { highlight, coin }) ->
                    group
                        [ cellBackgroundShape cfg
                        , coinToShape cfg coin
                            |> whenTrue highlight (applyHighlight cfg)
                        ]

                Nothing ->
                    cellBackgroundShape cfg

        viewCell : Pos -> Maybe CellView -> List Shape -> List Shape
        viewCell ( column, row ) maybeCellView =
            toCellShape maybeCellView
                |> move (GTransform.toScreenX column gt) (GTransform.toScreenY row gt)
                |> (::)
    in
    foldlGrid viewCell [] grid |> group



-- Main


main =
    game viewMemory updateMemory initialMemory
