module ConnectFourV3Kata1.Board exposing (Board, init, insert, positions)

import List.Extra


type Board
    = Board Rec


type alias Rec =
    { reverseMoves : List Int, width : Int, height : Int }


init : { a | width : Int, height : Int } -> Board
init { width, height } =
    Rec [] width height |> Board


insert : Int -> Board -> Board
insert column (Board rec) =
    Board { rec | reverseMoves = column :: rec.reverseMoves }


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
