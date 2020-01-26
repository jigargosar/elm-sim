module MirrorPuzzleV3.Graph exposing
    ( Edge
    , Graph
    , NodeType(..)
    , UnfoldInstruction(..)
    , create
    , edgeList
    , endPointList
    , unfold
    )

import Basics.Extra exposing (swap)
import Number2 exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import PointFree exposing (mapEach)
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
        insertNewEdge : Int2 -> Int2 -> { edges : Set Edge, eps : Set Int2 } -> Maybe { edges : Set Edge, eps : Set Int2 }
        insertNewEdge p1 p2 =
            Debug.todo "impl"

        toGraph : { edges : Set Edge, eps : Set Int2 } -> List ( Int2, Direction8 ) -> Graph
        toGraph acc vectors =
            case vectors of
                [] ->
                    Graph ( acc.edges, acc.eps )

                ( p1, d ) :: pending ->
                    let
                        p2 =
                            D.translatePoint p1 d
                    in
                    case typeOfNodeAt p2 of
                        Just ContinuePreviousDirectionNode ->
                            if Set.member ( p1, p2 ) acc.edges || Set.member ( p2, p1 ) acc.edges then
                                toGraph { acc | eps = Set.insert p2 acc.eps }
                                    (( p2, d ) :: pending)

                            else
                                toGraph { acc | edges = Set.insert ( p1, p2 ) acc.edges }
                                    (( p2, d ) :: pending)

                        Just (BranchNode dl) ->
                            if Set.member ( p1, p2 ) acc.edges || Set.member ( p2, p1 ) acc.edges then
                                toGraph { acc | eps = Set.insert p2 acc.eps }
                                    pending

                            else
                                toGraph { acc | edges = Set.insert ( p1, p2 ) acc.edges }
                                    (List.map (Tuple.pair p2) dl ++ pending)

                        Just LeafNode ->
                            if Set.member ( p1, p2 ) acc.edges || Set.member ( p2, p1 ) acc.edges then
                                toGraph { acc | eps = Set.insert p2 acc.eps } pending

                            else
                                toGraph { acc | edges = Set.insert ( p1, p2 ) acc.edges, eps = Set.insert p2 acc.eps } pending

                        Nothing ->
                            toGraph { acc | eps = Set.insert p1 acc.eps } pending
    in
    toGraph { edges = Set.empty, eps = Set.empty } (List.map (Tuple.pair startPoint) branchingDirections)


type UnfoldInstruction
    = ContinuePrevious
    | Stop
    | EndPoint
    | Fork (List Direction8)


unfold : (Int2 -> UnfoldInstruction) -> Int2 -> List Direction8 -> Graph
unfold unfoldInstructionAt startPosition startDirections =
    let
        nextSeeds : Seed -> List Seed
        nextSeeds ( position, directions ) =
            List.filterMap
                (\d ->
                    let
                        nextPosition =
                            D.translatePoint position d
                    in
                    case unfoldInstructionAt nextPosition of
                        ContinuePrevious ->
                            Just ( nextPosition, [ d ] )

                        Stop ->
                            Nothing

                        Fork nd ->
                            Just ( nextPosition, nd )

                        EndPoint ->
                            Just ( nextPosition, [] )
                )
                directions

        unfoldHelp : Context -> Graph
        unfoldHelp context0 =
            case popPending context0 of
                Nothing ->
                    Graph ( context0.edges, context0.endPoints )

                Just ( parentSeed, context ) ->
                    case context.nextSeeds parentSeed of
                        [] ->
                            unfoldHelp (insertEndPoint parentSeed context)

                        childSeeds ->
                            unfoldHelp
                                (List.foldl
                                    (updateContextForParentChildSeed parentSeed)
                                    context
                                    childSeeds
                                )
    in
    unfoldHelp (initContext nextSeeds ( startPosition, startDirections ))


updateContextForParentChildSeed : Seed -> Seed -> Context -> Context
updateContextForParentChildSeed parentSeed childSeed context =
    let
        edge =
            ( parentSeed, childSeed ) |> mapEach Tuple.first
    in
    if isEdgeMember edge context then
        -- on cyclic graph, we are currently adding an endpoint
        -- alternatively, we could just ignore ep, on cycle.
        insertEndPoint parentSeed context

    else
        insertEdge edge childSeed context



-- Unfolding Context


type alias Context =
    { edges : Set Edge
    , endPoints : Set Int2
    , nextSeeds : Seed -> List Seed
    , pendingSeeds : List Seed
    }


popPending : Context -> Maybe ( Seed, Context )
popPending context =
    case context.pendingSeeds of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, { context | pendingSeeds = rest } )


initContext : (Seed -> List Seed) -> Seed -> Context
initContext nextSeeds seed =
    Context Set.empty Set.empty nextSeeds [ seed ]


insertEndPoint : Seed -> Context -> Context
insertEndPoint ( parent, _ ) context =
    { context | endPoints = Set.insert parent context.endPoints }


insertEdge : Edge -> Seed -> Context -> Context
insertEdge edge seed context =
    { context | edges = Set.insert edge context.edges, pendingSeeds = seed :: context.pendingSeeds }


isEdgeMember : Edge -> Context -> Bool
isEdgeMember edge { edges } =
    Set.member edge edges || Set.member (swap edge) edges
