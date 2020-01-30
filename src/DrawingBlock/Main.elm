port module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events as BE
import DrawingBlock.Draggable as Draggable
import Html as H exposing (Html)
import IO
import Json.Decode as JD
import Number2 as N2 exposing (Float2, Int2)
import PointFree exposing (mapEach)
import Round
import String2 as S2
import Svg as S
import Svg.Attributes as SA
import Task


type alias DatGUIModel =
    { zoom : Float
    }


port onDatGUIChange : (DatGUIModel -> msg) -> Sub msg



-- Model


type alias Timestamp =
    Float


type alias EventData =
    ( Timestamp, Float2 )


type alias Model =
    { zoom : Float
    , scene : Float2
    , drag : Draggable.State
    , editMode : EditMode
    }


type EditMode
    = Zooming Float
    | NotEditing


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { zoom = 2.5
      , scene = ( 600, 600 )
      , drag = Draggable.intial
      , editMode = NotEditing
      }
    , IO.getBrowserWH
        |> Task.perform BrowserResized
    )



-- Update


type Msg
    = UpdateDrag Draggable.State Draggable.Event
    | StartDrag EditMode Draggable.State
    | BrowserResized Float2
    | OnKeyDown String
    | OnDatGUIChange DatGUIModel


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateDrag drag dragMsg ->
            ( { model | drag = drag } |> handleDragEvents dragMsg
            , Cmd.none
            )

        BrowserResized wh ->
            ( { model | scene = wh }, Cmd.none )

        OnKeyDown key ->
            ( case key of
                "1" ->
                    zoomIn model

                "2" ->
                    zoomOut model

                _ ->
                    model
            , Cmd.none
            )

        StartDrag editMode drag ->
            ( { model | editMode = editMode, drag = drag }, Cmd.none )

        OnDatGUIChange datGUIModel ->
            let
                _ =
                    Debug.log "datGUIModel" datGUIModel
            in
            ( { model | zoom = datGUIModel.zoom }, Cmd.none )


handleDragEvents : Draggable.Event -> Model -> Model
handleDragEvents event model =
    case ( event, model.editMode ) of
        ( Draggable.OnDrag { movementXY }, Zooming _ ) ->
            let
                ( _, dy ) =
                    movementXY

                zoomStep =
                    dy * 0.01 * model.zoom
            in
            { model | zoom = model.zoom + zoomStep }

        ( Draggable.OnUp _ _, _ ) ->
            { model | editMode = NotEditing }

        _ ->
            model


zoomIn model =
    { model | zoom = model.zoom |> (\s -> clamp 0.05 50 (s + s * 0.1)) }


zoomOut model =
    { model | zoom = model.zoom |> (\s -> clamp 0.05 50 (s + s * 0.1)) }


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions model =
    [ IO.onBrowserWH BrowserResized
    , JD.map OnKeyDown keyDecoder |> BE.onKeyDown
    , Draggable.subscriptions UpdateDrag model.drag
    , onDatGUIChange OnDatGUIChange
    ]
        |> Sub.batch



-- View


view : Model -> Html Msg
view model =
    H.div []
        [ IO.styleNode globalStyles
        , canvas model.scene model.zoom (viewState model)
        ]


globalStyles =
    """
    .ns-resize:hover {
        cursor: ns-resize;
    }

    .fill-green, .hover-fill-green:hover {
        fill: green;
    }
    """


viewState : Model -> List (S.Svg Msg)
viewState model =
    let
        isDraggingZoom =
            case model.editMode of
                Zooming _ ->
                    True

                _ ->
                    False
    in
    [ viewZoomData isDraggingZoom model.zoom
    ]


viewZoomData : Bool -> Float -> S.Svg Msg
viewZoomData forceHover zoom =
    [ IO.tspan "Zoom = " [ SA.fill "black" ]
    , IO.tspan (Round.round 2 zoom)
        []
    , S.style []
        (if forceHover then
            [ S.text
                """
                    body {
                        cursor: ns-resize;
                    }
                """
            ]

         else
            []
        )
    ]
        |> IO.textGroup
            [ Draggable.onMouseDown (StartDrag (Zooming zoom))
            , SA.class "ns-resize"
            , if forceHover then
                SA.class "fill-green"

              else
                SA.class "hover-fill-green"
            ]


canvas : Float2 -> Float -> List (S.Svg msg) -> Html msg
canvas browserWH zoom children =
    IO.canvas browserWH
        [ IO.group [ IO.transform [ IO.scale zoom ] ] children ]


empty : Html msg
empty =
    H.text ""



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
