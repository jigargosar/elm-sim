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
                    |> Result.map (withWinningPositions position coin)
                    |> Result.mapError convertError
            )
        |> Maybe.withDefault (Err NotSuccessful)


withWinningPositions : Position -> Coin -> Grid -> ( Set Position, Grid )
withWinningPositions position coin model =
    ( getWinningPositions position coin model, model )


getWinningPositions : Position -> Coin -> Grid -> Set Position
getWinningPositions startPosition coin (Grid grid) =
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

        moveBy : Position -> Position -> Position
        moveBy ( dx, dy ) ( x, y ) =
            ( x + dx, y + dy )

        validPositionsInDirection : Position -> List Position
        validPositionsInDirection direction =
            List.Extra.iterate (moveBy direction >> validatePosition) startPosition

        getWinningPositionsInOpposingDirections : Position -> Set Position
        getWinningPositionsInOpposingDirections direction =
            let
                connectedPositionSet =
                    Set.fromList
                        (validPositionsInDirection direction
                            ++ validPositionsInDirection (mapEach negate direction)
                        )
            in
            if Set.size connectedPositionSet >= 4 then
                connectedPositionSet

            else
                Set.empty

        directions =
            [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( -1, 1 ) ]

        reducer : Position -> Set Position -> Set Position
        reducer direction result =
            if Set.isEmpty result then
                getWinningPositionsInOpposingDirections direction

            else
                result
    in
    List.foldl reducer Set.empty directions


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
