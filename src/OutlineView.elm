module OutlineView exposing (viewSample)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import Maybe.Extra
import Pivot exposing (Pivot)
import String exposing (fromInt)


type alias OutlineView =
    { list : List Node
    , focused : Maybe NodeId
    }


type NodeId
    = NodeId String


type alias NodeData =
    { text : String
    , collapsed : Bool
    }


type Node
    = Node NodeId NodeData (List Node)


updateNode : NodeId -> (NodeData -> NodeData) -> OutlineView -> OutlineView
updateNode nodeId func ov =
    let
        updateNodeHelp (Node nid nd children) =
            if nid == nodeId then
                Node nid (func nd) children

            else
                Node nid nd (List.map updateNodeHelp children)
    in
    { ov | list = List.map updateNodeHelp ov.list }


nodeIdEq : NodeId -> Node -> Bool
nodeIdEq nodeId (Node nid _ _) =
    nodeId == nid


parentNodeId : NodeId -> OutlineView -> Maybe NodeId
parentNodeId nodeId ov =
    let
        parentNodeIdHelp (Node nid _ children) found =
            case found of
                Just _ ->
                    found

                Nothing ->
                    List.Extra.find (nodeIdEq nodeId) children
                        |> Maybe.map (\_ -> nid)
                        |> Maybe.Extra.orElseLazy (\_ -> List.foldl parentNodeIdHelp Nothing children)
    in
    List.foldl parentNodeIdHelp Nothing ov.list


initialOV : OutlineView
initialOV =
    let
        dataText : String -> NodeData
        dataText t =
            { text = t, collapsed = False }

        collapseND : NodeData -> NodeData
        collapseND nd =
            { nd | collapsed = True }
    in
    { list =
        [ Node (NodeId "10") (dataText "First Node") []
        , Node (NodeId "20") (dataText "Second Node") []
        , Node (NodeId "30")
            (dataText "Third Node ")
            [ Node (NodeId "30-10") (dataText "C1 Node ") []
            , Node (NodeId "30-20") (dataText "C2 Node ") []
            , Node (NodeId "30-25")
                (dataText "C2.5 ")
                [ Node (NodeId "30-25-10") (dataText "C1 Node ") []
                , Node (NodeId "30-25-20") (dataText "C2 Node ") []
                , Node (NodeId "30-25-30") (dataText "C3 Node ") []
                ]
            , Node (NodeId "30-30") (dataText "C3 Node ") []
            ]
        , Node (NodeId "40")
            (dataText "Fourth Node " |> collapseND)
            [ Node (NodeId "40-10") (dataText "C1 Node ") []
            , Node (NodeId "40-20") (dataText "C2 Node ") []
            , Node (NodeId "40-30") (dataText "C3 Node ") []
            ]
        ]
    , focused = Nothing
    }
        |> focusFirst


focusFirst : OutlineView -> OutlineView
focusFirst ov =
    let
        nvl =
            ovToNvList ov

        focused =
            Pivot.fromList nvl
                |> Maybe.andThen (\p -> Pivot.firstWith (.ancestorCollapsed >> not) p)
                |> Maybe.map (Pivot.getC >> .id >> NodeId)
    in
    { ov | focused = focused }


focusAfter : NodeId -> OutlineView -> OutlineView
focusAfter nid ov =
    let
        nvl =
            ovToNvList ov

        focused =
            Pivot.fromList nvl
                |> Maybe.andThen
                    (\p ->
                        Pivot.firstWith (.id >> NodeId >> (==) nid) p
                            |> Maybe.andThen (Pivot.findR (.ancestorCollapsed >> not))
                            |> Maybe.Extra.orElseLazy (\_ -> Pivot.lastWith (.ancestorCollapsed >> not) p)
                    )
                |> Maybe.map (Pivot.getC >> .id >> NodeId)
    in
    { ov | focused = focused }


focusLast : OutlineView -> OutlineView
focusLast ov =
    let
        nvl =
            ovToNvList ov

        focused =
            Pivot.fromList nvl
                |> Maybe.andThen (\p -> Pivot.lastWith (.ancestorCollapsed >> not) p)
                |> Maybe.map (Pivot.getC >> .id >> NodeId)
    in
    { ov | focused = focused }


focusBefore : NodeId -> OutlineView -> OutlineView
focusBefore nid ov =
    let
        nvl =
            ovToNvList ov

        focused =
            Pivot.fromList nvl
                |> Maybe.andThen
                    (\p ->
                        Pivot.firstWith (.id >> NodeId >> (==) nid) p
                            |> Maybe.andThen (Pivot.findL (.ancestorCollapsed >> not))
                            |> Maybe.Extra.orElseLazy (\_ -> Pivot.firstWith (.ancestorCollapsed >> not) p)
                    )
                |> Maybe.map (Pivot.getC >> .id >> NodeId)
    in
    { ov | focused = focused }


focusNext : OutlineView -> OutlineView
focusNext ov =
    case ov.focused of
        Nothing ->
            focusFirst ov

        Just nid ->
            focusAfter nid ov


focusPrev : OutlineView -> OutlineView
focusPrev ov =
    case ov.focused of
        Nothing ->
            focusLast ov

        Just nid ->
            focusBefore nid ov


hasExpandedChildren : NodeId -> OutlineView -> Bool
hasExpandedChildren nid ov =
    let
        nvl =
            ovToNvList ov
    in
    List.Extra.find (.id >> NodeId >> (==) nid) nvl
        |> Maybe.map (.collapseState >> (==) CS_Expanded)
        |> Maybe.withDefault False


hasCollapsedChildren : NodeId -> OutlineView -> Bool
hasCollapsedChildren nid ov =
    let
        nvl =
            ovToNvList ov
    in
    List.Extra.find (.id >> NodeId >> (==) nid) nvl
        |> Maybe.map (.collapseState >> (==) CS_Collapsed)
        |> Maybe.withDefault False


expandFocusedOrFocusNext : OutlineView -> OutlineView
expandFocusedOrFocusNext ov =
    case ov.focused of
        Just nid ->
            if hasCollapsedChildren nid ov then
                updateNode nid (\nd -> { nd | collapsed = False }) ov

            else
                focusNext ov

        Nothing ->
            ov


collapseFocusedOrFocusParentOrPrev : OutlineView -> OutlineView
collapseFocusedOrFocusParentOrPrev ov =
    case ov.focused of
        Just nid ->
            if hasExpandedChildren nid ov then
                updateNode nid (\nd -> { nd | collapsed = True }) ov

            else
                case parentNodeId nid ov of
                    Just pid ->
                        { ov | focused = Just pid }

                    Nothing ->
                        focusPrev ov

        Nothing ->
            ov


type CollapseState
    = CS_NoChildren
    | CS_Expanded
    | CS_Collapsed


type alias NodeView =
    { id : String
    , dataText : String
    , collapseState : CollapseState
    , ancestorCollapsed : Bool
    , level : Int
    , focused : Bool
    }


ovToNvList : OutlineView -> List NodeView
ovToNvList ov =
    let
        toCS data children =
            case ( data.collapsed, children ) of
                ( _, [] ) ->
                    CS_NoChildren

                ( True, _ ) ->
                    CS_Collapsed

                ( False, _ ) ->
                    CS_Expanded

        toNV level ancestorCollapsed (Node (NodeId id) data children) =
            let
                collapseState =
                    toCS data children
            in
            { id = id
            , dataText = data.text
            , collapseState = collapseState
            , ancestorCollapsed = ancestorCollapsed
            , level = level
            , focused = ov.focused == Just (NodeId id)
            }
                :: List.concatMap (toNV (level + 1) (collapseState == CS_Collapsed)) children
    in
    List.concatMap (toNV 0 False) ov.list


viewSample : Html msg
viewSample =
    let
        ov =
            initialOV
                |> focusNext
                |> focusNext
                |> focusNext
                |> focusNext
                |> focusNext
                |> collapseFocusedOrFocusParentOrPrev
                |> focusNext
                |> focusPrev
                |> expandFocusedOrFocusNext
                |> ovToNvList
    in
    div []
        [ div [ class "pv20" ] [ text "Outline View" ]
        , div [] (List.map viewNodeListItem ov)
        ]


viewNodeListItem : NodeView -> Html msg
viewNodeListItem nv =
    let
        { level, id, dataText, focused, collapseState } =
            nv
    in
    div
        ((style "padding-left" <| fromInt (level * 30) ++ "px")
            :: (if nv.ancestorCollapsed then
                    style "display" "none"

                else
                    class "df-row sp10"
               )
            :: (if focused then
                    [ style "outline-width" "1px"
                    , style "outline-color" "blue"
                    , style "outline-style" "auto"
                    ]

                else
                    []
               )
        )
        [ div [ class "" ]
            [ case collapseState of
                CS_NoChildren ->
                    text "_"

                CS_Expanded ->
                    text "-"

                CS_Collapsed ->
                    text "+"
            ]
        , text <| "id: " ++ id ++ ". data: " ++ dataText
        ]