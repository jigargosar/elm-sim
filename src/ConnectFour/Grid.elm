module ConnectFour.Grid exposing
    ( Cell
    , Coin(..)
    , Error(..)
    , GameOver(..)
    , Grid
    , Position
    , allPositions
    , clampPosition
    , columnScores
    , firstEmptyPositionInColumn
    , fromList
    , height
    , insertCoinInColumn
    , playableColumns
    , toList
    , width
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
clampPosition (Grid w h _) =
    Tuple.mapBoth (clamp 0 (w - 1)) (clamp 0 (h - 1))


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


insertCoinInColumn : Int -> Coin -> Grid -> Result Error ( Maybe GameOver, Grid )
insertCoinInColumn column coin model =
    firstEmptyPositionInColumn column model
        |> Maybe.map
            (\position ->
                insert position coin model
                    |> Result.map (withGameOver position coin)
            )
        |> Maybe.withDefault (Err NotSuccessful)


playableColumns : Grid -> Set Int
playableColumns =
    toCellList
        >> List.filterMap
            (\( ( x, _ ), cell ) ->
                if cell == Nothing then
                    Just x

                else
                    Nothing
            )
        >> Set.fromList


type alias Score =
    Int


columnScore : Int -> Grid -> Maybe Score
columnScore column grid =
    firstEmptyPositionInColumn column grid
        |> Maybe.map
            (\_ ->
                if column == centerColumn grid then
                    4

                else
                    0
            )


columnScores : Grid -> List ( Int, Maybe Score )
columnScores grid =
    playableColumns grid
        |> Set.toList
        |> List.map (\column -> ( column, columnScore column grid ))


centerColumn : Grid -> Int
centerColumn =
    width >> (toFloat >> (+) -1 >> (*) 0.5 >> floor)


type GameOver
    = WinningPositions (Set Position)
    | Draw


withGameOver : Position -> Coin -> Grid -> ( Maybe GameOver, Grid )
withGameOver position coin model =
    let
        winningPositions =
            getWinningPositions position coin model
    in
    ( if Set.isEmpty winningPositions then
        let
            isFilled =
                Dict.size (toDict model) == (width model * height model)
        in
        if isFilled then
            Just Draw

        else
            Nothing

      else
        WinningPositions winningPositions
            |> Just
    , model
    )


getWinningPositions : Position -> Coin -> Grid -> Set Position
getWinningPositions startPosition coin (Grid _ _ dict) =
    let
        validatePosition : Position -> Maybe Position
        validatePosition p =
            if Dict.get p dict == Just coin then
                Just p

            else
                Nothing

        moveBy : Position -> Position -> Position
        moveBy ( dx, dy ) ( x, y ) =
            ( x + dx, y + dy )

        connectedPositionsInDirection : Position -> List Position
        connectedPositionsInDirection direction =
            List.Extra.iterate (moveBy direction >> validatePosition) startPosition

        getWinningPositionsInOpposingDirections : Position -> Set Position
        getWinningPositionsInOpposingDirections direction =
            let
                connectedPositions =
                    Set.fromList
                        (connectedPositionsInDirection direction
                            ++ connectedPositionsInDirection (mapEach negate direction)
                        )
            in
            if Set.size connectedPositions >= 4 then
                connectedPositions

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


mapDict : (Dict Position Coin -> Dict Position Coin) -> Grid -> Grid
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


toList : Grid -> List ( Position, Coin )
toList =
    toDict >> Dict.toList


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


width : Grid -> Int
width (Grid w _ _) =
    w


height : Grid -> Int
height (Grid _ h _) =
    h
