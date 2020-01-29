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


type alias Model =
    { zoom : Float2
    , scene : Float2
    , drag : Drag.Model Element Float2
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { zoom = ( 1, 1 ) |> N2.scale 2.5
      , scene = ( 600, 600 )
      , drag = Drag.init
      }
    , IO.getBrowserWH
        |> Task.perform BrowserResized
    )



-- Update


type Msg
    = DragMsg (Drag.Msg Element Float2)
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
update message model =
    case message of
        DragMsg dragMsg ->
            ( onDragMessage dragMsg model, Cmd.none )

        BrowserResized wh ->
            ( { model | scene = wh }, Cmd.none )

        OnKeyDown key ->
            ( onKeyDown key model, Cmd.none )

        OnMouseDown element pageXY ->
            ( { model | drag = Drag.initDown element pageXY }, Cmd.none )


updateDrag : Drag.Msg Element Float2 -> Model -> Model
updateDrag message state =
    { state | drag = Drag.update message state.drag }


onDragMessage : Drag.Msg Element Float2 -> Model -> Model
onDragMessage message =
    updateDrag message
        >> handleDragEvents message


handleDragEvents : Drag.Msg Element Float2 -> Model -> Model
handleDragEvents message state =
    case message of
        Drag.OnClick ZoomElement _ ->
            { state | zoom = N2.scale 1.1 state.zoom }

        _ ->
            state


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


eventDecoder =
    JD.map2 Tuple.pair IO.timeStampDecoder IO.pageXYDecoder


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions model =
    [ IO.onBrowserWH BrowserResized
    , JD.map OnKeyDown keyDecoder |> BE.onKeyDown
    , Drag.subscriptions IO.pageXYDecoder model.drag |> Sub.map DragMsg
    ]
        |> Sub.batch



-- View


view : Model -> Html Msg
view model =
    canvas model.scene model.zoom (viewState model)


viewState model =
    [ viewZoomData model.zoom
    ]


viewZoomData : Float2 -> S.Svg Msg
viewZoomData zoom =
    let
        twoDecimalZoom =
            zoom |> mapEach (Round.round 2 >> String.toFloat |> ignoreNothing)
    in
    [ IO.tspan "Zoom = " []
    , IO.tspan (Debug.toString twoDecimalZoom)
        [ SA.id "zoom-element"
        , SE.on "down" (JD.map (OnMouseDown ZoomElement) IO.pageXYDecoder)
        , SA.class "pointer"
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
