module ConnectFour.Grid exposing
    ( Cell
    , Coin(..)
    , Grid
    , Position
    , clampPosition
    , dimensions
    , empty
    , firstEmptyPositionInColumn
    , fromList
    , putCoinInColumn
    , toCellList
    )

import Dict exposing (Dict)
import Grid.Bordered as Grid
import List.Extra
import PointFree exposing (flip, mapEach)
import Set exposing (Set)


type Coin
    = Red
    | Yellow


type alias Cell =
    Maybe Coin


type Grid
    = Grid GridModel


type alias GridModel =
    Grid.Grid Coin


type alias Position =
    ( Int, Int )


unwrap : Grid -> GridModel
unwrap (Grid grid) =
    grid


clampPosition : Grid -> Position -> Position
clampPosition =
    dimensions >> (\{ width, height } -> Tuple.mapBoth (clamp 0 (width - 1)) (clamp 0 (height - 1)))


empty : Int -> Int -> Grid
empty w h =
    Grid.empty { columns = w, rows = h } |> Grid


fromList : Int -> Int -> List ( Position, Coin ) -> Grid
fromList w h =
    Grid.fromList { columns = w, rows = h } >> Grid


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


type Error
    = OutOfBounds
    | NotSuccessful


convertError : Grid.Error -> Error
convertError error =
    case error of
        Grid.OutOfBounds ->
            OutOfBounds

        Grid.NotSuccessful ->
            NotSuccessful


putCoinInColumn : Int -> Coin -> Grid -> Result Error ( Set Position, Grid )
putCoinInColumn column coin model =
    firstEmptyPositionInColumn column model
        |> Maybe.map
            (\position ->
                insert position coin model
                    |> Result.map (withGameOverPositions position coin)
                    |> Result.mapError convertError
            )
        |> Maybe.withDefault (Err NotSuccessful)


withGameOverPositions : Position -> Coin -> Grid -> ( Set Position, Grid )
withGameOverPositions position coin model =
    ( getGameOverPositions position coin model, model )


getGameOverPositions : Position -> Coin -> Grid -> Set Position
getGameOverPositions startPosition coin (Grid grid) =
    let
        lookup : Dict Position Coin
        lookup =
            Grid.toDict grid

        validatePosition : Position -> Maybe Position
        validatePosition p =
            if Dict.get p lookup == Just coin then
                Just p

            else
                Nothing

        offsets =
            List.range 0 3

        winningSetSize =
            List.length offsets

        horizontalPositionSet =
            getConnectedPositionsInOpposingDirections ( 1, 0 ) validatePosition startPosition

        verticalPositionSet =
            getConnectedPositionsInOpposingDirections ( 0, 1 ) validatePosition startPosition

        getWinningPositionsInDirection dir =
            let
                connectedPositionSet =
                    getConnectedPositionsInOpposingDirections dir validatePosition startPosition
            in
            if Set.size connectedPositionSet >= 4 then
                connectedPositionSet

            else
                Set.empty

        directions =
            [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( -1, 1 ) ]

        reducer d acc =
            if Set.isEmpty acc then
                getWinningPositionsInDirection d

            else
                acc
    in
    List.foldl reducer Set.empty directions


getConnectedPositionsInOpposingDirections : Position -> (Position -> Maybe Position) -> Position -> Set Position
getConnectedPositionsInOpposingDirections direction validatePosition startPosition =
    let
        moveBy : Position -> Position -> Position
        moveBy ( dx, dy ) ( x, y ) =
            ( x + dx, y + dy )

        validPositionsInDirection : Position -> List Position
        validPositionsInDirection dir =
            List.Extra.iterate (moveBy dir >> validatePosition) startPosition
    in
    Set.fromList
        (validPositionsInDirection direction
            ++ validPositionsInDirection (mapEach negate direction)
        )


insert : Position -> Coin -> Grid -> Result Grid.Error Grid
insert position coin model =
    let
        grid =
            unwrap model
    in
    Grid.insert position coin grid |> Result.map Grid


columnEq : Int -> Position -> Bool
columnEq value ( column, _ ) =
    value == column


firstEmptyPositionInColumn : Int -> Grid -> Maybe Position
firstEmptyPositionInColumn column =
    unwrap >> Grid.emptyPositions >> List.Extra.find (columnEq column)


toCellList : Grid -> List ( Position, Cell )
toCellList =
    unwrap >> Grid.foldl (\p c -> (::) ( p, c )) []


dimensions : Grid -> { width : Int, height : Int }
dimensions =
    unwrap >> Grid.dimensions >> (\{ columns, rows } -> { width = columns, height = rows })
