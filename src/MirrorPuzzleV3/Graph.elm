module MirrorPuzzleV3.Graph exposing (..)

import Basics.Extra exposing (swap)
import Set exposing (Set)


type alias Graph comparable =
    ( Set (Edge comparable), Set comparable )


type alias Edge comparable =
    ( comparable, comparable )


type alias PathNode comparable a =
    ( comparable, a )


type alias NextPathNodes comparable a =
    PathNode comparable a -> List (PathNode comparable a)


unfoldGraph : NextPathNodes comparable a -> PathNode comparable a -> Graph comparable
unfoldGraph nextPathNodeContextsFunc =
    let
        gen : Graph comparable -> List (PathNode comparable a) -> Graph comparable
        gen acc0 nodeContexts0 =
            case nodeContexts0 of
                [] ->
                    acc0

                nodeContext0 :: nodeContexts1 ->
                    let
                        parent =
                            Tuple.first nodeContext0

                        nodeContexts2 : List (PathNode comparable a)
                        nodeContexts2 =
                            nextPathNodeContextsFunc nodeContext0

                        ( acc2, nodeContexts4 ) =
                            nodeContexts2
                                |> List.foldl
                                    (\nodeContext1 (( ( edges, endPoints ), nodeContexts3 ) as acc1) ->
                                        let
                                            child =
                                                Tuple.first nodeContext1

                                            edge =
                                                ( child, parent )
                                        in
                                        if Set.member edge edges || Set.member (swap edge) edges then
                                            -- on cyclic graph, we are currently adding an endpoint
                                            -- alternatively, we could just ignore ep, on cycle.
                                            ( ( edges, Set.insert parent endPoints ), nodeContexts3 )

                                        else
                                            ( ( Set.insert edge edges, endPoints ), nodeContext1 :: nodeContexts3 )
                                    )
                                    ( acc0, nodeContexts1 )
                    in
                    if List.isEmpty nodeContexts2 then
                        gen (Tuple.mapSecond (Set.insert parent) acc0) nodeContexts1

                    else
                        gen acc2 nodeContexts4
    in
    List.singleton >> gen ( Set.empty, Set.empty )
