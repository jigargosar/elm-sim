module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
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
    }


view : Model -> Html Msg
view _ =
    div []
        [ div [] [ text "Outline View" ]
        , div [] (List.concatMap (viewNode 0) ov.list)
        ]


viewNode level (Node (NodeId id) (NodeData data) children) =
    div
        [ style "padding-left" <| fromInt (level * 30) ++ "px"
        ]
        [ text <| "id: " ++ id ++ ". data: " ++ data
        ]
        :: List.concatMap (viewNode (level + 1)) children


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
