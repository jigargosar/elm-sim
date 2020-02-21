module LOL exposing (viewSample)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Group
    = Group String (List Item)


type Item
    = Item String


type alias Model =
    { groups : List Group }


viewGroupList : List Group -> Html msg
viewGroupList groups =
    let
        viewGroupTitle (Group title _) =
            div [] [ text title ]
    in
    List.map viewGroupTitle groups
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
        , viewGroupList sampleLOL
        ]
