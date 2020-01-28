module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events as BE
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


type Mouse
    = Up
    | Down EventData EventData


type Element
    = ZoomElement


type alias Env =
    { mouse : Mouse, scene : Float2 }


type Model
    = Model Env State


type alias State =
    { zoom : Float2
    , mouseOver : Maybe Element
    , mouseDown : Maybe ( Float2, Maybe Element )
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model (Env Up ( 600, 600 ))
        { zoom = ( 1, 1 ) |> N2.scale 2.5
        , mouseOver = Nothing
        , mouseDown = Nothing
        }
    , IO.getBrowserWH
        |> Task.perform BrowserResized
        |> Cmd.map EnvMsg
    )



-- Update


type StateMsg
    = MouseOverZoom
    | MouseOutZoom


type EnvMsg
    = BrowserResized Float2
    | OnMouseDown EventData
    | OnMouseUp EventData
    | OnMouseMove EventData
    | OnKeyDown String


type Msg
    = EnvMsg EnvMsg
    | StateMsg StateMsg


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
update message ((Model ({ mouse, scene } as env) state) as model) =
    case message of
        EnvMsg msg ->
            ( onEnvMessage msg env state, Cmd.none )

        StateMsg _ ->
            ( model, Cmd.none )


onEnvMessage : EnvMsg -> Env -> State -> Model
onEnvMessage message env state =
    -- ( Model (updateEnv message env) (updateStateOnEnvMsg message state), Cmd.none )
    case message of
        BrowserResized wh ->
            Model { env | scene = wh } state

        OnMouseDown e ->
            Model { env | mouse = Down e e } (onMouseDown state)

        OnMouseUp _ ->
            case env.mouse of
                Up ->
                    Model env state

                Down _ _ ->
                    Model { env | mouse = Up } (onMouseUp state)

        OnMouseMove current ->
            case env.mouse of
                Down start _ ->
                    Model { env | mouse = Down start current } state

                Up ->
                    Model env state

        OnKeyDown key ->
            Model env (onKeyDown key state)


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
subscriptions (Model env _) =
    Sub.batch
        [ IO.onBrowserWH BrowserResized
        , case env.mouse of
            Up ->
                BE.onMouseDown (JD.map OnMouseDown eventDecoder)

            Down _ _ ->
                [ BE.onMouseUp (JD.map OnMouseUp eventDecoder)
                , BE.onMouseMove (JD.map OnMouseMove eventDecoder)
                ]
                    |> Sub.batch
        , JD.map OnKeyDown keyDecoder
            |> BE.onKeyDown
        ]
        |> Sub.map EnvMsg



-- View


view : Model -> Html Msg
view (Model env state) =
    canvas env.scene state.zoom (viewState state)
        |> H.map StateMsg


viewState state =
    [ let
        isMouseOverZoom =
            state.mouseOver == Just ZoomElement
      in
      viewZoomData isMouseOverZoom state.zoom
    ]


viewZoomData : Bool -> Float2 -> S.Svg StateMsg
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
