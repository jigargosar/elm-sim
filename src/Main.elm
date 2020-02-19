module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
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
    , focused : Int
    }


type NodeId
    = NodeId String


type alias NodeData =
    { text : String
    , collapsed : Bool
    }


type Node
    = Node NodeId NodeData (List Node)


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
            (dataText "Third Node " |> collapseND)
            [ Node (NodeId "30-10") (dataText "C1 Node ") []
            , Node (NodeId "30-20") (dataText "C2 Node ") []
            , Node (NodeId "30-30") (dataText "C3 Node ") []
            ]
        ]
    , focused = 0
    }


moveFocusDown : OutlineView -> OutlineView
moveFocusDown ov =
    let
        nvList =
            ovToNv ov

        clampFocusIndex fi =
            if List.isEmpty nvList then
                0

            else
                clamp 0 (List.length nvList) fi
    in
    List.drop (ov.focused + 1) nvList
        |> List.Extra.findIndex (.ancestorCollapsed >> not)
        |> Maybe.map ((+) (ov.focused + 1) >> clampFocusIndex >> (\focused -> { ov | focused = focused }))
        |> Maybe.withDefault ov


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


ovToNv : OutlineView -> List NodeView
ovToNv ov =
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
            , focused = False
            }
                :: List.concatMap (toNV (level + 1) (collapseState == CS_Collapsed)) children
    in
    List.concatMap (toNV 0 False) ov.list
        |> List.Extra.updateAt ov.focused (\nv -> { nv | focused = True })


view : Model -> Html Msg
view _ =
    let
        ov =
            initialOV
                |> moveFocusDown
                |> moveFocusDown
                |> ovToNv
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



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
