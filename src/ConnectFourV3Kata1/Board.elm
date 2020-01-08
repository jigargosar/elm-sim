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
insert column (Board rec) =
    let
        columnHeight =
            List.Extra.count (is column) rec.reverseMoves
    in
    (if column >= 0 && column < rec.width && columnHeight < rec.height - 1 then
        { rec | reverseMoves = column :: rec.reverseMoves }

     else
        rec
    )
        |> Board


positions : Board -> List ( Int, Int )
positions (Board rec) =
    rec.reverseMoves
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.Extra.gatherEqualsBy Tuple.second
        |> List.concatMap
            (\( f, rest ) ->
                f
                    :: rest
                    |> List.indexedMap (\y ( i, x ) -> ( i, ( x, y ) ))
            )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
