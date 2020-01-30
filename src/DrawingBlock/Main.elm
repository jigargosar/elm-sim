port module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events as BE
import DrawingBlock.Drag as Drag exposing (DragModel)
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
    , drag : DragModel
    , editMode : EditMode
    }


type EditMode
    = Zooming Float2
    | NotEditing


type alias DragMsg =
    Drag.Msg


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { zoom = ( 1, 1 ) |> N2.scale 2.5
      , scene = ( 600, 600 )
      , drag = Drag.intial
      , editMode = NotEditing
      }
    , IO.getBrowserWH
        |> Task.perform BrowserResized
    )



-- Update


type Msg
    = OnDragMsg DragMsg
    | BrowserResized Float2
    | OnKeyDown String
    | OnMouseDown EditMode DragModel
    | OnDatGUIChange DatGUIModel


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnDragMsg dragMsg ->
            let
                ( drag, out ) =
                    Drag.update dragMsg model.drag
            in
            ( { model | drag = drag } |> handleDragEvents out
            , Cmd.none
            )

        BrowserResized wh ->
            ( { model | scene = wh }, Cmd.none )

        OnKeyDown key ->
            ( onKeyDown key model, Cmd.none )

        OnMouseDown editMode drag ->
            ( { model | editMode = editMode, drag = drag }, Cmd.none )

        OnDatGUIChange datGUIModel ->
            let
                _ =
                    Debug.log "datGUIModel" datGUIModel
            in
            ( model, Cmd.none )


handleDragEvents : Drag.OutMsg -> Model -> Model
handleDragEvents out model =
    case ( out, model.editMode ) of
        ( Drag.Move, Zooming _ ) ->
            let
                ( _, dy ) =
                    Drag.delta model.drag

                zoomStep =
                    ( dy, dy ) |> N2.scale 0.01 |> N2.mul model.zoom
            in
            { model | zoom = N2.add model.zoom zoomStep }

        _ ->
            model


onKeyDown : String -> Model -> Model
onKeyDown key state =
    let
        _ =
            Debug.log "key" key

        zoom =
            case key of
                "1" ->
                    state.zoom |> mapEach (\s -> clamp 0.05 50 (s + s * 0.1))

                "2" ->
                    state.zoom |> mapEach (\s -> clamp 0.05 50 (s - s * 0.1))

                _ ->
                    state.zoom
    in
    { state | zoom = zoom }


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions model =
    [ IO.onBrowserWH BrowserResized
    , JD.map OnKeyDown keyDecoder |> BE.onKeyDown
    , Drag.subscriptions model.drag |> Sub.map OnDragMsg
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
        , Drag.onDown (OnMouseDown (Zooming zoom))
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
