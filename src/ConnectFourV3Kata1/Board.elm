module ConnectFourV3Kata1.Board exposing
    ( Board
    , GameOver(..)
    , Player(..)
    , State(..)
    , info
    , init
    , insert
    )

import ConnectFourV3Kata1.Length as Len exposing (Length)
import Dict exposing (Dict)
import List.Extra
import PointFree exposing (allPass, anyPass, is, when)
import Set exposing (Set)


type Board
    = Board Rec


type Player
    = P1
    | P2


type GameOver
    = PlayerWon Player (Set ( Int, Int ))
    | Draw


type State
    = NextPlayer Player
    | GameOver GameOver


type alias Info =
    { dict : Dict ( Int, Int ) Player
    , state : State
    }


type alias Rec =
    { reverseMoves : List Int
    , width : Length
    , height : Length
    }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    { reverseMoves = []
    , width = Len.fromInt width
    , height = Len.fromInt height
    }
        |> Board


insert : Int -> Board -> Board
insert column =
    when (allPass [ columnNotFull column, gameNotOver ]) (appendMove column)


info : Board -> Info
info board =
    { dict = toDict board
    , state =
        case getGameOverState board of
            Just gameOverState ->
                GameOver gameOverState

            Nothing ->
                NextPlayer (nextPlayer board)
    }


gameNotOver : Board -> Bool
gameNotOver board =
    getGameOverState board == Nothing


getGameOverState : Board -> Maybe GameOver
getGameOverState board =
    if isDraw board then
        Just Draw

    else
        getLastMoveEntry board
            |> Maybe.andThen
                (\( pos, player ) ->
                    winningPositions pos player board
                        |> Maybe.map (PlayerWon player)
                )


winningPositions : ( Int, Int ) -> Player -> Board -> Maybe (Set ( Int, Int ))
winningPositions startPosition player board =
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
    positions board
        |> List.reverse
        |> List.head
        |> Maybe.map (\pos -> ( pos, flipPlayer (nextPlayer board) ))


flipPlayer : Player -> Player
flipPlayer player =
    case player of
        P1 ->
            P2

        P2 ->
            P1


positions : Board -> List ( Int, Int )
positions (Board rec) =
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


nextPlayer : Board -> Player
nextPlayer board =
    let
        ct =
            moveCount board
    in
    if modBy 2 ct == 0 then
        P1

    else
        P2


isDraw : Board -> Bool
isDraw board =
    moveCount board == maxMoves board


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


columnNotFull : Int -> Board -> Bool
columnNotFull column board =
    canInsertAt ( column, columnLength column board ) board


canInsertAt : ( Int, Int ) -> Board -> Bool
canInsertAt ( x, y ) =
    unwrap >> (\rec -> Len.member x rec.width && Len.member y rec.height)


columnLength : Int -> Board -> Int
columnLength column =
    unwrap >> (\rec -> List.Extra.count (is column) rec.reverseMoves)
