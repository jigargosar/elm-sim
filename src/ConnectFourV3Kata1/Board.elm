module ConnectFourV3Kata1.Board exposing
    ( Board
    , Player(..)
    , State(..)
    , init
    , insertInColumn
    , insertPositionFromColumn
    , state
    , toDict
    , transformState
    )

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


type InternalState
    = I_Turn Player
    | I_Victory Player (Set Pos)
    | I_Draw


type alias Rec =
    { width : Int
    , height : Int
    , dict : Dict ( Int, Int ) Player
    , state : InternalState
    }


type State
    = Turn Player
    | Victory Player (Set Pos)
    | Draw


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    { width = width
    , height = height
    , dict = Dict.empty
    , state = I_Turn P1
    }
        |> Board


insertInColumn : Int -> Board -> Board
insertInColumn x ((Board rec) as board) =
    case ( rec.state, insertPositionFromColumn x board ) of
        ( I_Turn player, Just pos ) ->
            { rec | dict = Dict.insert pos player rec.dict }
                |> updateState pos player
                |> Board

        _ ->
            board


insertPositionFromColumn : Int -> Board -> Maybe Pos
insertPositionFromColumn column (Board { width, height, dict }) =
    let
        row =
            dict |> Dict.keys >> List.Extra.count (Tuple.first >> is column)
    in
    if isIdxValid width column && isIdxValid height row then
        ( column, row ) |> Just

    else
        Nothing


isIdxValid : number -> number -> Bool
isIdxValid len idx =
    idx >= 0 && idx < len


transformState :
    { playerWon : Player -> Set ( Int, Int ) -> a
    , playerTurn : Player -> a
    , gameDraw : () -> a
    }
    -> Board
    -> a
transformState cb (Board rec) =
    case rec.state of
        I_Victory player winningPositions ->
            cb.playerWon player winningPositions

        I_Turn player ->
            cb.playerTurn player

        I_Draw ->
            cb.gameDraw ()


state : Board -> State
state =
    unwrap >> .state >> fromInternalState


fromInternalState : InternalState -> State
fromInternalState iState =
    case iState of
        I_Turn p ->
            Turn p

        I_Victory player set ->
            Victory player set

        I_Draw ->
            Draw


updateState : ( Int, Int ) -> Player -> Rec -> Rec
updateState pos player rec =
    let
        { width, height, dict } =
            rec
    in
    { rec
        | state =
            if Dict.size dict == width * height then
                I_Draw

            else
                case getWinningPositions pos player dict of
                    Just wp ->
                        I_Victory player wp

                    Nothing ->
                        I_Turn (flipPlayer player)
    }


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


unwrap : Board -> Rec
unwrap (Board rec) =
    rec


map : (Rec -> Rec) -> Board -> Board
map func =
    unwrap >> func >> Board
