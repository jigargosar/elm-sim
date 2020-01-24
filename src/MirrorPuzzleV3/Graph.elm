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
    Graph (unfoldGraphHelp getChildSeeds initialAcc)


unfoldGraphHelp :
    (( Int2, List Direction8 ) -> List Seed)
    -> ( ( Set Edge, Set Int2 ), List ( Int2, List Direction8 ) )
    -> ( Set Edge, Set Int2 )
unfoldGraphHelp getChildSeeds ( graphAcc, seedsAcc ) =
    case seedsAcc of
        [] ->
            graphAcc

        currentSeed :: remaningSeeds ->
            let
                parent : Int2
                parent =
                    Tuple.first currentSeed

                childSeeds : List Seed
                childSeeds =
                    getChildSeeds currentSeed
            in
            if List.isEmpty childSeeds then
                unfoldGraphHelp getChildSeeds ( insertEndPoint parent graphAcc, remaningSeeds )

            else
                unfoldGraphHelp getChildSeeds
                    (List.foldl
                        (accumGraphFor parent)
                        ( graphAcc, remaningSeeds )
                        childSeeds
                    )


accumGraphFor : Int2 -> Seed -> ( Acc, List Seed ) -> ( Acc, List Seed )
accumGraphFor parent childSeeds ( graphAcc, seedsAcc ) =
    let
        child =
            Tuple.first childSeeds

        edge =
            ( child, parent )
    in
    if isEdgeMember edge graphAcc then
        -- on cyclic graph, we are currently adding an endpoint
        -- alternatively, we could just ignore ep, on cycle.
        ( insertEndPoint parent graphAcc
        , seedsAcc
        )

    else
        ( insertEdge edge graphAcc
        , childSeeds :: seedsAcc
        )
