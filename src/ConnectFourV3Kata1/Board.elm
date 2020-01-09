module ConnectFourV3Kata1.Board exposing (Board, init, insert, positions)

import ConnectFourV3Kata1.Length as Length exposing (Length)
import List.Extra
import PointFree exposing (is, when)


type Board
    = Board Rec


type alias Rec =
    { reverseMoves : List Int, width : Length, height : Length }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    { reverseMoves = [], width = Length.init width, height = Length.init height } |> Board


insert : Int -> Board -> Board
insert column =
    when (validMove column) (appendMove column)


appendMove : Int -> Board -> Board
appendMove move (Board rec) =
    { rec | reverseMoves = move :: rec.reverseMoves }
        |> Board


validMove : Int -> Board -> Bool
validMove column (Board rec) =
    let
        row =
            List.Extra.count (is column) rec.reverseMoves
    in
    Length.member column rec.width && Length.member row rec.height


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
