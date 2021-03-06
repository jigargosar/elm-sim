module MirrorPuzzleV3.Graph exposing
    ( Edge
    , Graph
    , NodeType(..)
    , build
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


edgeList : Graph -> List Edge
edgeList =
    getEdges >> Set.toList


endPointList : Graph -> List Int2
endPointList =
    getEndPoints >> Set.toList


getEdges : Graph -> Set Edge
getEdges (Graph ( edges, _ )) =
    edges


getEndPoints : Graph -> Set Int2
getEndPoints (Graph ( _, eps )) =
    eps


type NodeType
    = ContinuePreviousDirectionNode
    | BranchNode (List Direction8)
    | LeafNode


build : (Int2 -> Maybe NodeType) -> Int2 -> List Direction8 -> Graph
build typeOfNodeAt startPoint branchingDirections =
    contextUnfolder (initContext typeOfNodeAt startPoint branchingDirections)



-- BuilderContext


type alias Context =
    { edges : Set Edge
    , eps : Set Int2
    , typeOfNodeAt : Int2 -> Maybe NodeType
    , pending : List ( Int2, Direction8 )
    }


initContext : (Int2 -> Maybe NodeType) -> Int2 -> List Direction8 -> Context
initContext typeOfNodeAt startPoint branchingDirections =
    Context Set.empty Set.empty typeOfNodeAt (List.map (Tuple.pair startPoint) branchingDirections)


contextUnfolder : Context -> Graph
contextUnfolder context0 =
    case popPending context0 of
        Nothing ->
            Graph ( context0.edges, context0.eps )

        Just ( ( p1, d ), context ) ->
            contextUnfolder (contextReducer p1 d context)


contextReducer : Int2 -> Direction8 -> Context -> Context
contextReducer p1 d acc =
    let
        p2 =
            D.translatePoint p1 d

        edge =
            ( p1, p2 )
    in
    case acc.typeOfNodeAt p2 of
        Just type_ ->
            case type_ of
                ContinuePreviousDirectionNode ->
                    if isEdgeMember edge acc then
                        insertEndPoint p1 acc

                    else
                        insertEdge edge acc |> pushPending ( p2, d )

                BranchNode dl ->
                    if isEdgeMember edge acc then
                        insertEndPoint p1 acc

                    else
                        insertEdge edge acc
                            |> pushPendingList (List.map (Tuple.pair p2) dl)

                LeafNode ->
                    if isEdgeMember edge acc then
                        insertEndPoint p2 acc

                    else
                        acc |> insertEndPoint p2 |> insertEdge edge

        Nothing ->
            insertEndPoint p1 acc


popPending : Context -> Maybe ( ( Int2, Direction8 ), Context )
popPending context =
    case context.pending of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, { context | pending = rest } )


pushPending : ( Int2, Direction8 ) -> Context -> Context
pushPending vec context =
    { context | pending = vec :: context.pending }


pushPendingList : List ( Int2, Direction8 ) -> Context -> Context
pushPendingList vecList context =
    { context | pending = vecList ++ context.pending }


insertEdge edge acc =
    { acc | edges = Set.insert edge acc.edges }


isEdgeMember ( p1, p2 ) acc =
    Set.member ( p1, p2 ) acc.edges || Set.member ( p2, p1 ) acc.edges


insertEndPoint ep acc =
    { acc | eps = Set.insert ep acc.eps }
