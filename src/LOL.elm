module LOL exposing (viewSample)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Group
    = Group String (List Item)


type Item
    = Item String


type alias Model =
    { groups : List Group }


viewGroupTitleList : List Group -> Html msg
viewGroupTitleList groups =
    let
        viewGroupTitle (Group title _) =
            div [] [ text title ]
    in
    List.map viewGroupTitle groups
        |> div []


viewGroupsWithItems : List Group -> Html msg
viewGroupsWithItems groups =
    let
        viewItem (Item title) =
            div [ class "pl3" ] [ text title ]

        viewGroupTitleAndItems (Group title items) =
            div [ class "pv2 b" ] [ text title ]
                :: List.map viewItem items
                |> div [ class "pv2" ]
    in
    List.map viewGroupTitleAndItems groups
        |> div []


viewSample : Html msg
viewSample =
    let
        ng =
            Group

        ni =
            Item

        sampleLOL =
            [ ng "Inbox"
                [ ni "x: Show Group title with items"
                , ni "x: should we show group list separately? and on nav show children? Or show entire tree?"
                , ni "x: For now it seems easier to show everything"
                , ni "x: now showing focus is complicated, since we are showing both, group &  it's items"
                , ni "x: perhaps not, we can have group-pivot, and for that group, item pivot. humm.."
                , ni "x: I think we should keep things simple. one view for master list, then sub view for its list items"
                , ni "how do we model group list view, group items view?"
                ]
            , ng "Projects"
                [ ni "LOL Demo"
                , ni "LOL: Focus Item/Group"
                ]
            ]
    in
    div [ class "pv2 ph4" ]
        [ div [ class "pv2 f4 b" ] [ text "LOL Demo" ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group with items" ]
            , viewGroupsWithItems sampleLOL
            ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group titles" ]
            , viewGroupTitleList sampleLOL
            ]
        ]
