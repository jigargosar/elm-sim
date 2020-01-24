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


insertEndPoint parent ( edges, endPoints ) =
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
unfoldGraphHelp getChildSeeds ( graphAcc, pendingSeeds ) =
    case pendingSeeds of
        [] ->
            Graph graphAcc

        currentSeed :: otherSeeds ->
            let
                parent : Int2
                parent =
                    Tuple.first currentSeed
            in
            case getChildSeeds currentSeed of
                [] ->
                    unfoldGraphHelp getChildSeeds ( insertEndPoint parent graphAcc, otherSeeds )

                childSeeds ->
                    unfoldGraphHelp getChildSeeds
                        (List.foldl
                            (accumGraphWithChildSeedOf parent)
                            ( graphAcc, otherSeeds )
                            childSeeds
                        )


accumGraphWithChildSeedOf : Int2 -> Seed -> ( Acc, List Seed ) -> ( Acc, List Seed )
accumGraphWithChildSeedOf parent childSeed ( graphAcc, pendingSeeds ) =
    let
        child =
            Tuple.first childSeed

        edge =
            ( child, parent )
    in
    if isEdgeMember edge graphAcc then
        -- on cyclic graph, we are currently adding an endpoint
        -- alternatively, we could just ignore ep, on cycle.
        ( insertEndPoint parent graphAcc
        , pendingSeeds
        )

    else
        ( insertEdge edge graphAcc
        , childSeed :: pendingSeeds
        )
