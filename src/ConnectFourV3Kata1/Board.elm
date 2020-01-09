module ConnectFourV3Kata1.Board exposing
    ( Board
    , Player(..)
    , init
    , insert
    , toDict
    , transformState
    )

import ConnectFourV3Kata1.Length as Len exposing (Length)
import Dict exposing (Dict)
import List.Extra
import PointFree exposing (is)
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
    { width : Length
    , height : Length
    , dict : Dict ( Int, Int ) Player
    , state : State
    }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    { width = Len.fromInt width
    , height = Len.fromInt height
    , dict = Dict.empty
    , state = Turn P1
    }
        |> Board


insert : Int -> Board -> Board
insert column ((Board rec) as board) =
    case ( rec.state, columnToInsertPosition column board ) of
        ( Turn player, Just pos ) ->
            insertAt pos player board

        _ ->
            board


transformState :
    { playerWon : Player -> Set ( Int, Int ) -> a
    , playerTurn : Player -> a
    , gameDraw : () -> a
    }
    -> Board
    -> a
transformState cb (Board rec) =
    case rec.state of
        Victory player winningPositions ->
            cb.playerWon player winningPositions

        Turn player ->
            cb.playerTurn player

        Draw ->
            cb.gameDraw ()


insertAt : Pos -> Player -> Board -> Board
insertAt pos player ((Board rec) as board) =
    let
        dict =
            Dict.insert pos player rec.dict

        state =
            if Dict.size dict == maxMoves board then
                Draw

            else
                case getWinningPositions pos player dict of
                    Just wp ->
                        Victory player wp

                    Nothing ->
                        Turn (flipPlayer player)
    in
    { rec | dict = dict, state = state } |> Board


columnToInsertPosition : Int -> Board -> Maybe Pos
columnToInsertPosition column (Board { width, height, dict }) =
    let
        row =
            dict |> Dict.keys >> List.Extra.count (Tuple.first >> is column)
    in
    if Len.member column width && Len.member row height then
        ( column, row ) |> Just

    else
        Nothing


getWinningPositions : ( Int, Int ) -> Player -> Dict Pos Player -> Maybe (Set ( Int, Int ))
getWinningPositions startPosition player dict =
    let
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


flipPlayer : Player -> Player
flipPlayer player =
    case player of
        P1 ->
            P2

        P2 ->
            P1


toDict : Board -> Dict ( Int, Int ) Player
toDict (Board rec) =
    rec.dict


maxMoves : Board -> Int
maxMoves (Board { width, height }) =
    Len.toInt width * Len.toInt height


unwrap : Board -> Rec
unwrap (Board rec) =
    rec


map : (Rec -> Rec) -> Board -> Board
map func =
    unwrap >> func >> Board


isValidRow : Int -> Board -> Bool
isValidRow row =
    unwrap >> (\rec -> Len.member row rec.height)
