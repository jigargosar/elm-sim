module MirrorPuzzleV3.Graph exposing
    ( Edge
    , Graph
    , UnfoldInstruction(..)
    , getEdges
    , getEndPoints
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


getEndPoints : Graph -> Set Int2
getEndPoints (Graph ( _, leafNodes )) =
    leafNodes


type alias Seed =
    ( Int2, List Direction8 )


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
                            D.stepPos position d
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
    in
    unfoldHelp (initContext nextSeeds ( startPosition, startDirections ))


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
