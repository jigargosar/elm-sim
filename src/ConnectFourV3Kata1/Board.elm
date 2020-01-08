module ConnectFourV3Kata1.Board exposing (Board)

import Basics.Extra exposing (uncurry)
import Dict
import List.Extra
import PointFree exposing (pairTo)


type Board
    = Board Rec


type alias Rec =
    { moves : List Int, width : Int, height : Int }


foldl func acc0 (Board rec) =
    let
        reducer column acc =
            acc

        foo =
            List.indexedMap (\i column -> ( column, modBy 2 i == 0 )) [ 0, 1, 0, 1, 1, 1, 1 ]
                |> List.Extra.gatherEqualsBy Tuple.first
                |> List.concatMap
                    (\( f, rest ) ->
                        f
                            :: rest
                            |> List.indexedMap (\r ( c, bool ) -> ( ( c, r ), bool ))
                    )
                |> List.foldl (uncurry Dict.insert) Dict.empty

        _ =
            1
    in
    List.foldl reducer acc0 rec.moves
