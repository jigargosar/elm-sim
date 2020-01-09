module ConnectFourV3Kata1.Board exposing
    ( Board
    , Player(..)
    , init
    , insert
    , mapState
    , toDict
    )

import ConnectFourV3Kata1.Length as Len exposing (Length)
import Dict exposing (Dict)
import List.Extra
import PointFree exposing (allPass, is, when)
import Set exposing (Set)


type Board
    = Board Rec


type Player
    = P1
    | P2


type alias Pos =
    ( Int, Int )


type State
    = Turn Player
    | Victory Player (Set Pos)
    | Draw


type alias Rec =
    { reverseMoves : List Int
    , width : Length
    , height : Length
    , dict : Dict ( Int, Int ) Player
    , state : State
    }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    { reverseMoves = []
    , width = Len.fromInt width
    , height = Len.fromInt height
    , dict = Dict.empty
    , state = Turn P1
    }
        |> Board


insert : Int -> Board -> Board
insert column ((Board rec) as board) =
    let
        _ =
            case rec.state of
                Turn player ->
                    let
                        _ =
                            columnToInsertPosition column board
                    in
                    when (canInsertInColumn column)
                        (appendMove column)
                        board

                Victory player set ->
                    board

                Draw ->
                    board
    in
    when (allPass [ gameNotWon, canInsertInColumn column ])
        (appendMove column)
        board


columnToInsertPosition : Int -> Board -> Maybe Pos
columnToInsertPosition column board =
    if isValidColumn column board then
        let
            row =
                columnLength column board
        in
        if isValidRow row board then
            ( column, row ) |> Just

        else
            Nothing

    else
        Nothing


type alias Callbacks a =
    { playerWon : Player -> Set ( Int, Int ) -> a
    , playerTurn : Player -> a
    , gameDraw : () -> a
    }


mapState : Callbacks a -> Board -> a
mapState cb board =
    case getPlayerWon board of
        Just ( player, winningPositions ) ->
            cb.playerWon player winningPositions

        Nothing ->
            case playerTurnAtMoveIdx (moveCount board) board of
                Just nextPlayer ->
                    cb.playerTurn nextPlayer

                Nothing ->
                    cb.gameDraw ()


gameNotWon : Board -> Bool
gameNotWon board =
    getPlayerWon board == Nothing


getPlayerWon : Board -> Maybe ( Player, Set ( Int, Int ) )
getPlayerWon board =
    getLastMoveEntry board
        |> Maybe.andThen
            (\( pos, player ) ->
                getWinningPositions pos player board
                    |> Maybe.map (Tuple.pair player)
            )


getWinningPositions : ( Int, Int ) -> Player -> Board -> Maybe (Set ( Int, Int ))
getWinningPositions startPosition player board =
    let
        dict =
            toDict board

        stepPosition ( dx, dy ) ( x, y ) =
            ( x + dx, y + dy )

        negateStep ( dx, dy ) =
            ( -dx, -dy )

        connectedPositionsHelp step position acc =
            let
                nextPosition =
                    stepPosition step position
            in
            if Dict.get nextPosition dict == Just player then
                connectedPositionsHelp step nextPosition (Set.insert nextPosition acc)

            else
                acc

        connectedPositions step =
            connectedPositionsHelp step startPosition Set.empty

        connectedOpposingPositions step =
            connectedPositions step
                |> Set.union (connectedPositions (negateStep step))
                |> Set.insert startPosition
    in
    [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ) ]
        |> List.map connectedOpposingPositions
        |> List.Extra.find (Set.size >> is 4)


getLastMoveEntry : Board -> Maybe ( ( Int, Int ), Player )
getLastMoveEntry board =
    Maybe.map2 Tuple.pair (lastMovePosition board) (playerTurnAtMoveIdx (moveCount board - 1) board)


lastMove : Board -> Maybe Int
lastMove =
    unwrap >> .reverseMoves >> List.head


lastMovePosition : Board -> Maybe ( Int, Int )
lastMovePosition board =
    lastMove board |> Maybe.map (\column -> ( column, columnLength column board - 1 ))


flipPlayer : Player -> Player
flipPlayer player =
    case player of
        P1 ->
            P2

        P2 ->
            P1


toDict : Board -> Dict ( Int, Int ) Player
toDict (Board rec) =
    rec.reverseMoves
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.Extra.gatherEqualsBy Tuple.second
        |> List.concatMap
            (\( first, rest ) ->
                (first :: rest)
                    |> List.indexedMap (\y ( idx, x ) -> ( idx, ( x, y ) ))
            )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> List.foldl
            (\position ( player, acc ) ->
                ( flipPlayer player, Dict.insert position player acc )
            )
            ( P1, Dict.empty )
        |> Tuple.second


playerTurnAtMoveIdx : Int -> Board -> Maybe Player
playerTurnAtMoveIdx idx board =
    if idx < 0 || idx >= maxMoves board then
        Nothing

    else if modBy 2 idx == 0 then
        Just P1

    else
        Just P2


maxMoves : Board -> Int
maxMoves (Board { width, height }) =
    Len.toInt width * Len.toInt height


moveCount : Board -> Int
moveCount =
    unwrap >> .reverseMoves >> List.length


unwrap : Board -> Rec
unwrap (Board rec) =
    rec


map : (Rec -> Rec) -> Board -> Board
map func =
    unwrap >> func >> Board


appendMove : Int -> Board -> Board
appendMove move =
    map (\rec -> { rec | reverseMoves = move :: rec.reverseMoves })


canInsertInColumn : Int -> Board -> Bool
canInsertInColumn column board =
    isValidColumn column board
        && isValidRow (columnLength column board) board


isValidRow : Int -> Board -> Bool
isValidRow row =
    unwrap >> (\rec -> Len.member row rec.height)


isValidColumn : Int -> Board -> Bool
isValidColumn column =
    unwrap >> (\rec -> Len.member column rec.width)


columnLength : Int -> Board -> Int
columnLength column =
    unwrap >> (\rec -> List.Extra.count (is column) rec.reverseMoves)
