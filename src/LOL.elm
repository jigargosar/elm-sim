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
                [ ni "Show Group Items"
                ]
            , ng "Projects"
                [ ni "LOL Demo"
                ]
            ]
    in
    div [ class "pv2 ph4" ]
        [ div [ class "pv2 f4 b" ] [ text "LOL Demo" ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group With Items" ]
            , viewGroupsWithItems sampleLOL
            ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group Titles" ]
            , viewGroupTitleList sampleLOL
            ]
        ]
