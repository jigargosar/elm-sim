module MirrorPuzzleV3.Graph exposing (Graph, getEdges, getLeafNodes, unfoldGraph)

import Basics.Extra exposing (swap)
import Number2 exposing (Int2)
import Playground.Direction8 exposing (Direction8)
import PointFree exposing (mapEach)
import Set exposing (Set)


type Graph
    = Graph ( Set Edge, Set Int2 )


type alias Edge =
    ( Int2, Int2 )


getEdges : Graph -> Set Edge
getEdges (Graph ( edges, _ )) =
    edges


getLeafNodes : Graph -> Set Int2
getLeafNodes (Graph ( _, leafNodes )) =
    leafNodes


type alias Seed =
    ( Int2, List Direction8 )


unfoldGraph :
    (Seed -> List Seed)
    -> Seed
    -> Graph
unfoldGraph nextSeeds seed =
    unfoldGraphHelp ( initContext nextSeeds, [ seed ] )


unfoldGraphHelp : ( Context, List Seed ) -> Graph
unfoldGraphHelp ( context, pendingSeeds ) =
    case pendingSeeds of
        [] ->
            Graph ( context.edges, context.endPoints )

        currentSeed :: otherSeeds ->
            case context.nextSeeds currentSeed of
                [] ->
                    unfoldGraphHelp ( insertEndPoint currentSeed context, otherSeeds )

                childSeeds ->
                    unfoldGraphHelp
                        (List.foldl
                            (updateContextForParentChildSeed currentSeed)
                            ( context, otherSeeds )
                            childSeeds
                        )


updateContextForParentChildSeed : Seed -> Seed -> ( Context, List Seed ) -> ( Context, List Seed )
updateContextForParentChildSeed parentSeed childSeed ( context, pendingSeeds ) =
    let
        edge =
            ( parentSeed, childSeed ) |> mapEach Tuple.first
    in
    if isEdgeMember edge context then
        -- on cyclic graph, we are currently adding an endpoint
        -- alternatively, we could just ignore ep, on cycle.
        ( insertEndPoint parentSeed context
        , pendingSeeds
        )

    else
        ( insertEdge edge context
        , childSeed :: pendingSeeds
        )



-- Unfold Context


type alias Context =
    { edges : Set Edge
    , endPoints : Set Int2
    , nextSeeds : Seed -> List Seed
    }


initContext : (Seed -> List Seed) -> Context
initContext nextSeeds =
    Context Set.empty Set.empty nextSeeds


insertEndPoint : Seed -> Context -> Context
insertEndPoint ( parent, _ ) context =
    { context | endPoints = Set.insert parent context.endPoints }


insertEdge : Edge -> Context -> Context
insertEdge edge context =
    { context | edges = Set.insert edge context.edges }


isEdgeMember : Edge -> Context -> Bool
isEdgeMember edge { edges } =
    Set.member edge edges || Set.member (swap edge) edges
