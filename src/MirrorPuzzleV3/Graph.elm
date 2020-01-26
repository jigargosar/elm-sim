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

                        isEdgeMember =
                            Set.member ( p1, p2 ) acc.edges || Set.member ( p2, p1 ) acc.edges

                        recurse acc_ =
                            toGraph acc_ pending

                        recurse1 acc_ newVec =
                            toGraph acc_ (newVec :: pending)

                        recurseList acc_ newList =
                            toGraph acc_ (newList ++ pending)

                        insertEdge acc_ =
                            { acc_ | edges = Set.insert ( p1, p2 ) acc_.edges }
                    in
                    case typeOfNodeAt p2 of
                        Just ContinuePreviousDirectionNode ->
                            if isEdgeMember then
                                recurse { acc | eps = Set.insert p1 acc.eps }

                            else
                                recurse1 (insertEdge acc) ( p2, d )

                        Just (BranchNode dl) ->
                            if isEdgeMember then
                                recurse { acc | eps = Set.insert p1 acc.eps }

                            else
                                recurseList (insertEdge acc)
                                    (List.map (Tuple.pair p2) dl)

                        Just LeafNode ->
                            if isEdgeMember then
                                recurse { acc | eps = Set.insert p2 acc.eps }

                            else
                                recurse
                                    { acc
                                        | edges = Set.insert ( p1, p2 ) acc.edges
                                        , eps = Set.insert p2 acc.eps
                                    }

                        Nothing ->
                            recurse { acc | eps = Set.insert p1 acc.eps }

        -- recurse acc
    in
    toGraph { edges = Set.empty, eps = Set.empty } (List.map (Tuple.pair startPoint) branchingDirections)
