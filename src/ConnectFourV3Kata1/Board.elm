module ConnectFourV3Kata1.Board exposing (Board, init, insert, positions)

import List.Extra
import PointFree exposing (is)


type Board
    = Board Rec


type alias Rec =
    { reverseMoves : List Int, width : Int, height : Int }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    Rec [] width height |> Board


insert : Int -> Board -> Board
insert column ((Board rec) as board) =
    (if isValidInsertColumn column board then
        { rec | reverseMoves = column :: rec.reverseMoves }

     else
        rec
    )
        |> Board


isValidInsertColumn : Int -> Board -> Bool
isValidInsertColumn column (Board rec) =
    let
        columnHeight =
            List.Extra.count (is column) rec.reverseMoves
    in
    column >= 0 && column < rec.width && columnHeight < rec.height - 1


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
