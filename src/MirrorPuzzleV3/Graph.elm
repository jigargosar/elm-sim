module MirrorPuzzleV3.Graph exposing
    ( Graph
    , Movement(..)
    , getEdges
    , getEndPoints
    , unfoldDirection8Graph
    , unfoldGraph
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


type Movement
    = Continue
    | End
    | Split (List Direction8)


unfoldDirection8Graph : (Int2 -> Movement) -> Int2 -> Maybe Graph
unfoldDirection8Graph movementAt startPosition =
    let
        nextSeeds : Seed -> List Seed
        nextSeeds ( position, directions ) =
            List.filterMap
                (\d ->
                    let
                        nextPosition =
                            D.stepPos position d
                    in
                    case movementAt nextPosition of
                        Continue ->
                            Just ( nextPosition, [ d ] )

                        End ->
                            Nothing

                        Split nd ->
                            Just ( nextPosition, nd )
                )
                directions
    in
    case movementAt startPosition of
        Continue ->
            Nothing

        End ->
            Nothing

        Split nd ->
            Just (unfoldGraph nextSeeds ( startPosition, nd ))


unfoldGraph :
    (Seed -> List Seed)
    -> Seed
    -> Graph
unfoldGraph nextSeeds seed =
    unfoldGraphHelp (initContext nextSeeds seed)


unfoldGraphHelp : Context -> Graph
unfoldGraphHelp context0 =
    case popPending context0 of
        Nothing ->
            Graph ( context0.edges, context0.endPoints )

        Just ( parentSeed, context ) ->
            case context.nextSeeds parentSeed of
                [] ->
                    unfoldGraphHelp (insertEndPoint parentSeed context)

                childSeeds ->
                    unfoldGraphHelp
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
