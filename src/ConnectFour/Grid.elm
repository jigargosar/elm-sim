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
    , height
    , insertCoinInColumn
    , playableColumns
    , toCellList
    , toList
    , width
    , withInitialMoves
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


flipCoin : Coin -> Coin
flipCoin coin =
    if coin == Red then
        Yellow

    else
        Red


withInitialMoves : Int -> Int -> Coin -> List Int -> Result Error ( Coin, Grid )
withInitialMoves w h =
    let
        reducer : Int -> Result Error ( Coin, ( a, Grid ) ) -> Result Error ( Coin, ( Maybe GameOver, Grid ) )
        reducer column =
            Result.andThen
                (\( coin, ( _, grid ) ) ->
                    insertCoinInColumn column coin grid
                        |> Result.map (Tuple.pair (flipCoin coin))
                )
    in
    \coin moves ->
        List.foldl reducer (Ok ( coin, ( Nothing, empty w h ) )) moves
            |> Result.map (Tuple.mapSecond Tuple.second)


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


playablePositions : Grid -> List (Maybe Position)
playablePositions grid =
    List.Extra.initialize (width grid) (\column -> firstEmptyPositionInColumn column grid)


type alias Score =
    Int


positionScore : Position -> Coin -> Grid -> Score
positionScore startPosition coin grid =
    let
        ( startColumn, _ ) =
            startPosition

        centerScore =
            if startColumn == centerColumn grid then
                4

            else
                0

        cellsInDir : Position -> List (Maybe Coin)
        cellsInDir dir =
            List.Extra.unfoldr
                (\( n, p ) ->
                    if n == 4 then
                        Nothing

                    else
                        let
                            nextPosition =
                                moveBy dir p
                        in
                        case get nextPosition grid of
                            Err _ ->
                                Nothing

                            Ok cell ->
                                if cell == Nothing || cell == Just coin then
                                    Just ( cell, ( n + 1, nextPosition ) )

                                else
                                    Nothing
                )
                ( 1, startPosition )

        scoreInOpposingDirs : Position -> Int
        scoreInOpposingDirs dir =
            let
                dirCells =
                    cellsInDir dir

                opposingDirCells =
                    cellsInDir (mapEach negate dir)

                allCells =
                    List.reverse opposingDirCells ++ [ Just coin ] ++ dirCells

                dirScore =
                    if List.isEmpty dirCells then
                        0

                    else
                        cellsToScore coin (allCells |> List.reverse)

                oppScore =
                    if List.isEmpty opposingDirCells then
                        0

                    else
                        cellsToScore coin allCells

                log val =
                    let
                        logVal =
                            [ List.reverse opposingDirCells, [ Just coin ], dirCells ]

                        _ =
                            if dir == ( 1, 0 ) && startColumn == 2 then
                                Debug.log "val" logVal

                            else
                                logVal
                    in
                    val
            in
            log (dirScore + oppScore)

        directions =
            [ ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( -1, 1 ) ]

        directionScores =
            List.map scoreInOpposingDirs directions
                |> List.sum
    in
    centerScore + directionScores


cellsToScore : Coin -> List (Maybe Coin) -> number
cellsToScore coin list_ =
    let
        list =
            list_ |> List.take 4

        _ =
            List.Extra.groupsOfWithStep 4 1
    in
    if List.length list == 4 then
        case List.Extra.count ((==) (Just coin)) list of
            2 ->
                2

            3 ->
                5

            4 ->
                1000

            _ ->
                0

    else
        0


columnScores : Coin -> Grid -> List ( Int, Maybe Score )
columnScores coin grid =
    playablePositions grid
        |> List.indexedMap
            (\column maybePosition ->
                ( column, Maybe.map (\position -> positionScore position coin grid) maybePosition )
            )


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


moveBy : Position -> Position -> Position
moveBy ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


getWinningPositions : Position -> Coin -> Grid -> Set Position
getWinningPositions startPosition coin (Grid _ _ dict) =
    let
        validatePosition : Position -> Maybe Position
        validatePosition p =
            if Dict.get p dict == Just coin then
                Just p

            else
                Nothing

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


get : Position -> Grid -> Result Error Cell
get position model =
    if isValid position model then
        Dict.get position (toDict model) |> Ok

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
