module MirrorPuzzleV3.Graph exposing (..)

import Basics.Extra exposing (swap)
import Set exposing (Set)


type alias Graph node =
    ( List (Edge node), List node )


type alias Edge node =
    ( node, node )


type alias GraphAcc node comparable =
    ( Graph node, Acc comparable )


type alias Acc comparable =
    ( Set (Edge comparable), Set comparable )


insertEndPoint parentNode parent ( ( edgeList, endPointList ), ( edges, endPoints ) ) =
    ( ( edgeList, parentNode :: endPointList ), ( edges, Set.insert parent endPoints ) )


insertEdge nodeEdge edge ( ( edgeList, endPointList ), ( edges, endPoints ) ) =
    ( ( nodeEdge :: edgeList, endPointList ), ( Set.insert edge edges, endPoints ) )


isEdgeMember edge ( ( _, _ ), ( edges, _ ) ) =
    Set.member edge edges || Set.member (swap edge) edges


unfoldGraph :
    { nextNodes : node -> List node
    , toComparable : node -> comparable
    }
    -> node
    -> GraphAcc node comparable
unfoldGraph cfg =
    let
        accumGraphFor parentNode parent childNode ( gAcc, nodes ) =
            let
                child =
                    cfg.toComparable childNode

                edge =
                    ( child, parent )

                nodeEdge =
                    ( childNode, parentNode )
            in
            if isEdgeMember edge gAcc then
                -- on cyclic graph, we are currently adding an endpoint
                -- alternatively, we could just ignore ep, on cycle.
                ( insertEndPoint parentNode parent gAcc
                , nodes
                )

            else
                ( insertEdge nodeEdge edge gAcc
                , childNode :: nodes
                )

        nextGraphAcc : GraphAcc node comparable -> List node -> GraphAcc node comparable
        nextGraphAcc gAcc0 nodes0 =
            case nodes0 of
                [] ->
                    gAcc0

                parentNode :: remaningParentNodes ->
                    let
                        parent : comparable
                        parent =
                            cfg.toComparable parentNode

                        childNodes : List node
                        childNodes =
                            cfg.nextNodes parentNode
                    in
                    if List.isEmpty childNodes then
                        nextGraphAcc (insertEndPoint parentNode parent gAcc0) remaningParentNodes

                    else
                        let
                            ( gAcc1, nodes1 ) =
                                List.foldl
                                    (accumGraphFor parentNode parent)
                                    ( gAcc0, remaningParentNodes )
                                    childNodes
                        in
                        nextGraphAcc gAcc1 nodes1
    in
    List.singleton >> nextGraphAcc ( ( [], [] ), ( Set.empty, Set.empty ) )
