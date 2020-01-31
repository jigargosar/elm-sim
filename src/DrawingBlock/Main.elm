port module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events as BE
import DrawingBlock.Draggable as Draggable
import Html as H exposing (Html)
import IO
import Json.Decode as JD
import Number2 as NT exposing (Float2, Int2)
import Round
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
    , pan : Float2
    , scene : Float2
    , draggableState : Draggable.State
    , dragging : Maybe Dragging
    }


type Dragging
    = Zooming
    | Panning


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { zoom = 2.5
      , pan = ( 0, 0 )
      , scene = ( 600, 600 )
      , draggableState = Draggable.intial
      , dragging = Nothing
      }
    , IO.getBrowserSize |> Task.perform GotBrowserSize
    )



-- Update


type Msg
    = UpdateDrag Draggable.State Draggable.Event
    | StartDrag Dragging Draggable.State IO.MouseEvent
    | GotBrowserSize Float2
    | OnKeyDown String
    | OnDatGUIChange DatGUIModel


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        StartDrag dragging draggableState _ ->
            ( { model | dragging = Just dragging, draggableState = draggableState }, Cmd.none )

        UpdateDrag draggableState draggableMsg ->
            ( { model | draggableState = draggableState } |> handleDragEvents draggableMsg
            , Cmd.none
            )

        GotBrowserSize wh ->
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

        OnDatGUIChange datGUIModel ->
            let
                _ =
                    Debug.log "datGUIModel" datGUIModel
            in
            ( { model | zoom = datGUIModel.zoom }, Cmd.none )


handleDragEvents : Draggable.Event -> Model -> Model
handleDragEvents event model =
    case ( event, model.dragging ) of
        ( Draggable.OnDrag { movementXY }, Just Zooming ) ->
            let
                ( _, dy ) =
                    movementXY |> NT.negateSecond

                zoomStep =
                    dy * 0.01 * model.zoom

                newZoom =
                    model.zoom + zoomStep
            in
            setZoomUpdatePan newZoom model

        ( Draggable.OnDrag { movementXY }, Just Panning ) ->
            let
                panStep =
                    movementXY |> NT.negateSecond |> NT.scale (1 / model.zoom)
            in
            { model | pan = NT.add model.pan panStep }

        ( Draggable.OnEnd _ _, _ ) ->
            { model | dragging = Nothing }

        _ ->
            model


setZoomUpdatePan zoom model =
    { model
        | zoom = zoom
        , pan = model.pan |> NT.scale model.zoom |> NT.scale (1 / zoom)
    }


zoomIn model =
    setZoomUpdatePan (model.zoom |> (\s -> clamp 0.05 50 (s + s * 0.1))) model


zoomOut model =
    setZoomUpdatePan (model.zoom |> (\s -> clamp 0.05 50 (s + s * 0.1))) model


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions model =
    [ IO.onBrowserResize GotBrowserSize
    , JD.map OnKeyDown keyDecoder |> BE.onKeyDown
    , Draggable.subscriptions UpdateDrag model.draggableState
    , onDatGUIChange OnDatGUIChange
    ]
        |> Sub.batch



-- View


view : Model -> Html Msg
view model =
    H.div []
        [ IO.styleNode globalStyles
        , IO.canvas model.scene
            [ Draggable.onMouseDown (StartDrag Panning)
            ]
            [ IO.group [ IO.transform [ IO.scale model.zoom, IO.shift model.pan ] ]
                (drawing model)
            ]
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


drawing : Model -> List (S.Svg Msg)
drawing model =
    let
        isDraggingZoom =
            model.dragging == Just Zooming
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
            [ Draggable.onMouseDown (StartDrag Zooming)
            , SA.class "ns-resize"
            , if forceHover then
                SA.class "fill-green"

              else
                SA.class "hover-fill-green"
            ]


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
