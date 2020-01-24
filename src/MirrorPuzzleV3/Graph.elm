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
unfoldGraph nextSeeds =
    let
        accumGraphFor : Int2 -> Seed -> ( Acc, List Seed ) -> ( Acc, List Seed )
        accumGraphFor parent childNode ( gAcc, nodes ) =
            let
                child =
                    Tuple.first childNode

                edge =
                    ( child, parent )
            in
            if isEdgeMember edge gAcc then
                -- on cyclic graph, we are currently adding an endpoint
                -- alternatively, we could just ignore ep, on cycle.
                ( insertEndPoint parent gAcc
                , nodes
                )

            else
                ( insertEdge edge gAcc
                , childNode :: nodes
                )

        nextGraphAcc : Acc -> List Seed -> Acc
        nextGraphAcc gAcc0 nodes0 =
            case nodes0 of
                [] ->
                    gAcc0

                parentNode :: remaningParentNodes ->
                    let
                        parent : Int2
                        parent =
                            Tuple.first parentNode

                        childNodes : List Seed
                        childNodes =
                            nextSeeds parentNode
                    in
                    if List.isEmpty childNodes then
                        nextGraphAcc (insertEndPoint parent gAcc0) remaningParentNodes

                    else
                        let
                            ( gAcc1, nodes1 ) =
                                List.foldl
                                    (accumGraphFor parent)
                                    ( gAcc0, remaningParentNodes )
                                    childNodes
                        in
                        nextGraphAcc gAcc1 nodes1
    in
    List.singleton >> nextGraphAcc ( Set.empty, Set.empty ) >> Graph
