module MirrorPuzzleV3.Graph exposing (Graph, getEdges, getLeafNodes, unfoldGraph)

import Basics.Extra exposing (swap)
import Number2 exposing (Int2)
import Playground.Direction8 exposing (Direction8)
import Set exposing (Set)


type Graph
    = Graph Acc


type alias NodeGraph node =
    ( List Edge, List node )


type alias Edge =
    ( Int2, Int2 )


getEdges : Graph -> Set Edge
getEdges (Graph ( edges, _ )) =
    edges


getLeafNodes : Graph -> Set Int2
getLeafNodes (Graph ( _, leafNodes )) =
    leafNodes


type alias Acc =
    ( Set Edge, Set Int2 )


insertEndPoint ( parent, _ ) ( edges, endPoints ) =
    ( edges, Set.insert parent endPoints )


insertEdge edge ( edges, endPoints ) =
    ( Set.insert edge edges, endPoints )


isEdgeMember edge ( edges, _ ) =
    Set.member edge edges || Set.member (swap edge) edges


type alias Seed =
    ( Int2, List Direction8 )


unfoldGraph :
    (Seed -> List Seed)
    -> Seed
    -> Graph
unfoldGraph getChildSeeds seed =
    let
        initialAcc =
            ( ( Set.empty, Set.empty ), List.singleton seed )
    in
    unfoldGraphHelp getChildSeeds initialAcc


unfoldGraphHelp :
    (Seed -> List Seed)
    -> ( Acc, List Seed )
    -> Graph
unfoldGraphHelp getNextSeeds ( graphAcc, pendingSeeds ) =
    case pendingSeeds of
        [] ->
            Graph graphAcc

        currentSeed :: otherSeeds ->
            case getNextSeeds currentSeed of
                [] ->
                    unfoldGraphHelp getNextSeeds ( insertEndPoint currentSeed graphAcc, otherSeeds )

                childSeeds ->
                    unfoldGraphHelp getNextSeeds
                        (List.foldl
                            (accumGraphWithChildSeedOf currentSeed)
                            ( graphAcc, otherSeeds )
                            childSeeds
                        )


accumGraphWithChildSeedOf : Seed -> Seed -> ( Acc, List Seed ) -> ( Acc, List Seed )
accumGraphWithChildSeedOf parentSeed childSeed ( graphAcc, pendingSeeds ) =
    let
        child =
            Tuple.first childSeed

        edge =
            ( child, Tuple.first parentSeed )
    in
    if isEdgeMember edge graphAcc then
        -- on cyclic graph, we are currently adding an endpoint
        -- alternatively, we could just ignore ep, on cycle.
        ( insertEndPoint parentSeed graphAcc
        , pendingSeeds
        )

    else
        ( insertEdge edge graphAcc
        , childSeed :: pendingSeeds
        )
