module ConnectFourV3Kata1.Board exposing
    ( Board
    , Player(..)
    , init
    , insertInColumn
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


type State
    = Turn Player
    | Victory Player (Set Pos)
    | Draw


type alias Rec =
    { width : Int
    , height : Int
    , dict : Dict ( Int, Int ) Player
    , state : State
    }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    { width = width
    , height = height
    , dict = Dict.empty
    , state = Turn P1
    }
        |> Board


insertInColumn : Int -> Board -> Board
insertInColumn x ((Board rec) as board) =
    let
        y =
            rec.dict |> Dict.keys >> List.Extra.count (Tuple.first >> is x)

        pos =
            ( x, y )
    in
    case rec.state of
        Turn player ->
            insertAt pos player rec |> Board

        _ ->
            board


insertAt : Pos -> Player -> Rec -> Rec
insertAt pos player rec =
    let
        { width, height, dict } =
            rec

        ( x, y ) =
            pos

        isPositionWithinBounds =
            x >= 0 && x < width && y >= 0 && y < height
    in
    if isPositionWithinBounds then
        { rec | dict = Dict.insert pos player dict }
            |> updateState pos player

    else
        rec


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


updateState : ( Int, Int ) -> Player -> Rec -> Rec
updateState pos player rec =
    let
        { width, height, dict } =
            rec
    in
    { rec
        | state =
            if Dict.size dict == width * height then
                Draw

            else
                case getWinningPositions pos player dict of
                    Just wp ->
                        Victory player wp

                    Nothing ->
                        Turn (flipPlayer player)
    }


columnToDictPosition : Int -> Board -> Maybe Pos
columnToDictPosition column (Board { width, height, dict }) =
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
