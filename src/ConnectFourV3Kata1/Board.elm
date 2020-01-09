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
import PointFree exposing (is, when)
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
    when (canInsertIn column) (addMove column)


info : Board -> Info
info board =
    { dict = toDict board
    , state =
        if isDraw board then
            GameOver Draw

        else
            NextPlayer (nextPlayer board)
    }


flipPlayer : Player -> Player
flipPlayer player =
    case player of
        P1 ->
            P2

        P2 ->
            P1


toDict : Board -> Dict ( Int, Int ) Player
toDict =
    positions
        >> List.foldl
            (\position ( player, acc ) ->
                ( flipPlayer player, Dict.insert position player acc )
            )
            ( P1, Dict.empty )
        >> Tuple.second


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


addMove : Int -> Board -> Board
addMove move =
    map (\rec -> { rec | reverseMoves = move :: rec.reverseMoves })


canInsertIn : Int -> Board -> Bool
canInsertIn column board =
    canInsertAt ( column, columnLength column board ) board


canInsertAt : ( Int, Int ) -> Board -> Bool
canInsertAt ( x, y ) =
    unwrap >> (\rec -> Len.member x rec.width && Len.member y rec.height)


columnLength : Int -> Board -> Int
columnLength column =
    unwrap >> (\rec -> List.Extra.count (is column) rec.reverseMoves)


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
