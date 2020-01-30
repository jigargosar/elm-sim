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
    { zoom : Float2
    , scene : Float2
    , drag : Draggable.State
    , editMode : EditMode
    }


type EditMode
    = Zooming Float2
    | NotEditing


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { zoom = ( 1, 1 ) |> N2.scale 2.5
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
            ( { model | zoom = datGUIModel.zoom |> N2.singleton }, Cmd.none )


handleDragEvents : Draggable.Event -> Model -> Model
handleDragEvents event model =
    case ( event, model.editMode ) of
        ( Draggable.OnDrag { movementXY }, Zooming _ ) ->
            let
                ( _, dy ) =
                    movementXY

                zoomStep =
                    ( dy, dy ) |> N2.scale 0.01 |> N2.mul model.zoom
            in
            { model | zoom = N2.add model.zoom zoomStep }

        ( Draggable.OnUp _ _, _ ) ->
            { model | editMode = NotEditing }

        _ ->
            model


zoomIn model =
    { model | zoom = model.zoom |> mapEach (\s -> clamp 0.05 50 (s + s * 0.1)) }


zoomOut model =
    { model | zoom = model.zoom |> mapEach (\s -> clamp 0.05 50 (s + s * 0.1)) }


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
    canvas model.scene model.zoom (viewState model)


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


viewZoomData : Bool -> Float2 -> S.Svg Msg
viewZoomData forceHover zoom =
    let
        twoDecimalZoom =
            zoom |> mapEach (Round.round 2) |> S2.join " , "
    in
    [ IO.tspan "Zoom = " []
    , IO.tspan (Debug.toString twoDecimalZoom)
        [ SA.id "zoom-element"
        , Draggable.onMouseDown (StartDrag (Zooming zoom))
        , SA.class "pointer"
        , if forceHover then
            SA.style """
                        fill: green;
                        cursor: ns-resize;
                     """

          else
            SA.style """
                     """
        ]
    , S.style []
        [ S.text
            """
                #zoom-element:hover{
                    fill: green;
                    cursor: ns-resize;
                }
            """
        ]
    ]
        |> IO.textGroup []


canvas : Float2 -> ( Float, Float ) -> List (S.Svg msg) -> Html msg
canvas browserWH zoom children =
    IO.canvas browserWH
        [ IO.group [ IO.transform [ IO.scale2 zoom ] ] children ]


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
