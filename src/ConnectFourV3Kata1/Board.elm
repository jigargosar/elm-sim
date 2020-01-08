module ConnectFourV3Kata1.Board exposing (Board)

import Basics.Extra exposing (uncurry)
import Dict
import List.Extra
import PointFree exposing (pairTo)


type Board
    = Board Rec


type alias Rec =
    { moves : List Int, width : Int, height : Int }


positions : Board -> List ( Int, Int )
positions (Board rec) =
    List.indexedMap Tuple.pair rec.moves
        |> List.Extra.gatherEqualsBy Tuple.second
        |> List.concatMap
            (\( f, rest ) ->
                f
                    :: rest
                    |> List.indexedMap (\y ( i, x ) -> ( i, ( x, y ) ))
            )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
