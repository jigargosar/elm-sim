module MirrorPuzzleV3.Graph exposing
    ( Edge
    , Graph
    , NodeType(..)
    , create
    , edgeList
    , endPointList
    )

import Number2 exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import Set exposing (Set)


type Graph
    = Graph ( Set Edge, Set Int2 )


type alias Edge =
    ( Int2, Int2 )


getEdges : Graph -> Set Edge
getEdges (Graph ( edges, _ )) =
    edges


edgeList : Graph -> List Edge
edgeList =
    getEdges >> Set.toList


endPointList : Graph -> List Int2
endPointList =
    getEndPoints >> Set.toList


getEndPoints : Graph -> Set Int2
getEndPoints (Graph ( _, leafNodes )) =
    leafNodes


type alias Seed =
    ( Int2, List Direction8 )


type NodeType
    = ContinuePreviousDirectionNode -- Or we can Skip node
    | BranchNode (List Direction8)
    | LeafNode


insertEdge edge acc =
    { acc | edges = Set.insert edge acc.edges }


isEdgeMember ( p1, p2 ) acc =
    Set.member ( p1, p2 ) acc.edges || Set.member ( p2, p1 ) acc.edges


insertEndPoint ep acc =
    { acc | eps = Set.insert ep acc.eps }


create : (Int2 -> Maybe NodeType) -> Int2 -> List Direction8 -> Graph
create typeOfNodeAt startPoint branchingDirections =
    let
        toGraph : { edges : Set Edge, eps : Set Int2 } -> List ( Int2, Direction8 ) -> Graph
        toGraph acc vectors =
            case vectors of
                [] ->
                    Graph ( acc.edges, acc.eps )

                ( p1, d ) :: pending ->
                    let
                        p2 =
                            D.translatePoint p1 d

                        edge =
                            ( p1, p2 )
                    in
                    case typeOfNodeAt p2 of
                        Just ContinuePreviousDirectionNode ->
                            if isEdgeMember edge acc then
                                toGraph (insertEndPoint p1 acc) pending

                            else
                                toGraph (insertEdge edge acc) (( p2, d ) :: pending)

                        Just (BranchNode dl) ->
                            if isEdgeMember edge acc then
                                toGraph (insertEndPoint p1 acc) pending

                            else
                                toGraph (insertEdge edge acc)
                                    (List.map (Tuple.pair p2) dl ++ pending)

                        Just LeafNode ->
                            if isEdgeMember edge acc then
                                toGraph (insertEndPoint p2 acc) pending

                            else
                                toGraph
                                    (acc |> insertEndPoint p2 |> insertEdge edge)
                                    pending

                        Nothing ->
                            toGraph (insertEndPoint p1 acc) pending

        -- recurse acc
    in
    toGraph { edges = Set.empty, eps = Set.empty } (List.map (Tuple.pair startPoint) branchingDirections)
