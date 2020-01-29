module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events as BE
import DrawingBlock.Drag as Drag
import Html as H exposing (Html)
import IO
import Json.Decode as JD
import Number2 as N2 exposing (Float2, Int2)
import PointFree exposing (ignoreNothing, mapEach)
import Round
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Task



-- Model


type alias Timestamp =
    Float


type alias EventData =
    ( Timestamp, Float2 )


type Element
    = ZoomElement


type alias Env =
    { scene : Float2 }


type Model
    = Model Env State


type alias State =
    { zoom : Float2
    , mouseOver : Maybe Element
    , mouseDown : Maybe ( Float2, Maybe Element )
    , drag : Drag.Model Element Float2
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model (Env ( 600, 600 ))
        { zoom = ( 1, 1 ) |> N2.scale 2.5
        , mouseOver = Nothing
        , mouseDown = Nothing
        , drag = Drag.init
        }
    , IO.getBrowserWH
        |> Task.perform BrowserResized
    )



-- Update


type Msg
    = MouseOverZoom
    | MouseOutZoom
    | DragMsg (Drag.Msg Element Float2)
    | BrowserResized Float2
    | OnKeyDown String
    | OnMouseDown Element Float2


type alias EventDiff =
    { dx : Float
    , dy : Float
    , isDragging : Bool
    }


eventDiff : EventData -> EventData -> EventDiff
eventDiff ( st, sxy ) ( t, xy ) =
    let
        elapsed =
            ( t, st ) |> uncurry (-)

        tooLong =
            abs elapsed > (3 * 1000)

        dx =
            ( xy, sxy ) |> mapEach Tuple.first |> uncurry (-)

        dy =
            ( xy, sxy ) |> mapEach Tuple.second |> uncurry (-)

        tooFar =
            abs dx > 10 || abs dy > 10

        isDragging =
            tooLong || tooFar
    in
    EventDiff dx dy isDragging


update : Msg -> Model -> ( Model, Cmd Msg )
update message ((Model ({ scene } as env) state) as model) =
    case message of
        MouseOverZoom ->
            ( model, Cmd.none )

        MouseOutZoom ->
            ( model, Cmd.none )

        DragMsg dragMsg ->
            ( Model env (onDragMessage dragMsg state), Cmd.none )

        BrowserResized wh ->
            ( Model { env | scene = wh } state, Cmd.none )

        OnKeyDown key ->
            ( Model env (onKeyDown key state), Cmd.none )

        OnMouseDown element pageXY ->
            ( Model env { state | drag = Drag.initDown element pageXY }, Cmd.none )


updateDrag : Drag.Msg Element Float2 -> State -> State
updateDrag message state =
    { state | drag = Drag.update message state.drag }


onDragMessage : Drag.Msg Element Float2 -> State -> State
onDragMessage message =
    updateDrag message
        >> handleDragEvents message


handleDragEvents : Drag.Msg Element Float2 -> State -> State
handleDragEvents message state =
    case message of
        Drag.OnClick ZoomElement _ ->
            { state | zoom = N2.scale 1.1 state.zoom }

        _ ->
            state


onMouseDown : State -> State
onMouseDown state =
    { state | mouseDown = Just ( state.zoom, state.mouseOver ) }


onMouseUp : State -> State
onMouseUp state =
    { state | mouseDown = Nothing }


onKeyDown : String -> State -> State
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


eventDecoder =
    JD.map2 Tuple.pair IO.timeStampDecoder IO.pageXYDecoder


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions (Model env state) =
    [ IO.onBrowserWH BrowserResized
    , JD.map OnKeyDown keyDecoder |> BE.onKeyDown
    , Drag.subscriptions IO.pageXYDecoder state.drag |> Sub.map DragMsg
    ]
        |> Sub.batch



-- View


view : Model -> Html Msg
view (Model env state) =
    canvas env.scene state.zoom (viewState state)


viewState state =
    [ let
        isMouseOverZoom =
            state.mouseOver == Just ZoomElement
      in
      viewZoomData isMouseOverZoom state.zoom
    ]


viewZoomData : Bool -> Float2 -> S.Svg Msg
viewZoomData isMouseOver zoom =
    let
        twoDecimalZoom =
            zoom |> mapEach (Round.round 2 >> String.toFloat |> ignoreNothing)
    in
    [ IO.tspan "Zoom = " []
    , IO.tspan (Debug.toString twoDecimalZoom)
        [ SA.id "zoom-element"
        , SE.onMouseOver MouseOverZoom
        , SE.onMouseOut MouseOutZoom
        , SE.on "down" (JD.map (OnMouseDown ZoomElement) IO.pageXYDecoder)
        , SA.fill
            (if isMouseOver then
                "green"

             else
                ""
            )
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
