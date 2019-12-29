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
import List.Extra
import PointFree exposing (mapEach)
import Set exposing (Set)


type Coin
    = Red
    | Yellow


type alias Cell =
    Maybe Coin


type alias Position =
    ( Int, Int )


type Grid
    = Grid Int Int (Dict Position Coin)


clampPosition : Grid -> Position -> Position
clampPosition =
    dimensions >> (\{ width, height } -> Tuple.mapBoth (clamp 0 (width - 1)) (clamp 0 (height - 1)))


empty : Int -> Int -> Grid
empty w h =
    Grid w h Dict.empty


fromList : Int -> Int -> List ( Position, Coin ) -> Grid
fromList w h =
    let
        ignoreError : (Grid -> Result x Grid) -> Grid -> Grid
        ignoreError func model =
            func model |> Result.withDefault model
    in
    List.foldl (\( position, coin ) -> ignoreError (insert position coin)) (empty w h)


type Error
    = OutOfBounds
    | NotSuccessful


putCoinInColumn : Int -> Coin -> Grid -> Result Error ( Set Position, Grid )
putCoinInColumn column coin model =
    firstEmptyPositionInColumn column model
        |> Maybe.map
            (\position ->
                insert position coin model
                    |> Result.map (withWinningPositions position coin)
            )
        |> Maybe.withDefault (Err NotSuccessful)


withWinningPositions : Position -> Coin -> Grid -> ( Set Position, Grid )
withWinningPositions position coin model =
    ( getWinningPositions position coin model, model )


getWinningPositions : Position -> Coin -> Grid -> Set Position
getWinningPositions startPosition coin (Grid _ _ grid) =
    let
        lookup : Dict Position Coin
        lookup =
            grid

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


insert : Position -> Coin -> Grid -> Result Error Grid
insert position coin model =
    if isValid position model then
        mapDict (Dict.insert position coin) model |> Ok

    else
        Err OutOfBounds


mapDict func (Grid w h dict) =
    func dict |> Grid w h


isValid : ( Int, Int ) -> Grid -> Bool
isValid ( x, y ) (Grid w h _) =
    x >= 0 && x < w && y >= 0 && y < h


firstEmptyPositionInColumn : Int -> Grid -> Maybe Position
firstEmptyPositionInColumn column =
    toCellList
        >> List.Extra.find (\( ( x, _ ), cell ) -> x == column && cell == Nothing)
        >> Maybe.map Tuple.first


toDict : Grid -> Dict Position Coin
toDict (Grid _ _ dict) =
    dict


toCellList : Grid -> List ( Position, Cell )
toCellList model =
    let
        dict =
            toDict model

        pairPositionWithCell : Position -> ( Position, Cell )
        pairPositionWithCell position =
            ( position, Dict.get position dict )
    in
    List.map pairPositionWithCell (allPositions model)


allPositions : Grid -> List Position
allPositions (Grid w h _) =
    List.Extra.initialize w (Tuple.pair >> List.Extra.initialize h)
        |> List.concat


toWH (Grid w h _) =
    ( w, h )


dimensions : Grid -> { width : Int, height : Int }
dimensions =
    toWH >> (\( width, height ) -> { width = width, height = height })
