module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import String exposing (fromInt)



-- Model


type alias Model =
    {}


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


type alias OutlineView =
    { list : List Node
    , focused : NodeId
    }


type NodeId
    = NodeId String


type NodeData
    = NodeData String


type Node
    = Node NodeId NodeData (List Node)


ov : OutlineView
ov =
    { list =
        [ Node (NodeId "10") (NodeData "First Node") []
        , Node (NodeId "20") (NodeData "Second Node") []
        , Node (NodeId "30")
            (NodeData "Third Node ")
            [ Node (NodeId "30-10") (NodeData "C1 Node ") []
            , Node (NodeId "30-20") (NodeData "C2 Node ") []
            , Node (NodeId "30-30") (NodeData "C3 Node ") []
            ]
        ]
    , focused = NodeId "30"
    }


type alias NodeView =
    { id : String
    , dataText : String
    , level : Int
    , focused : Bool
    }


nvList : List NodeView
nvList =
    let
        toNV level (Node (NodeId id) (NodeData data) children) =
            NodeView id data level (NodeId id == ov.focused)
                :: List.concatMap (toNV (level + 1)) children
    in
    List.concatMap (toNV 0) ov.list


view : Model -> Html Msg
view _ =
    div []
        [ div [ class "pv20" ] [ text "Outline View" ]
        , div [] (List.map viewNodeListItem nvList)
        ]


viewNodeListItem : NodeView -> Html msg
viewNodeListItem { level, id, dataText, focused } =
    div
        ((style "padding-left" <| fromInt (level * 30) ++ "px")
            :: (if focused then
                    [ style "outline-width" "1px"
                    , style "outline-color" "blue"
                    , style "outline-style" "auto"
                    ]

                else
                    []
               )
        )
        [ text <| "id: " ++ id ++ ". data: " ++ dataText
        ]


empty : Html msg
empty =
    Html.text ""



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
