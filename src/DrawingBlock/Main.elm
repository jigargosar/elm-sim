module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Events as BE
import Html exposing (Html)
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


type Env
    = Env Mouse Float2


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
    , IO.getBrowserWH |> Task.perform BrowserResized
      --, Cmd.none
    )



-- Update


type Msg
    = NoOp
    | BrowserResized Float2
    | OnMouseDown EventData
    | OnMouseUp EventData
    | OnMouseMove EventData
    | MouseOverZoom
    | MouseOutZoom
    | OnKeyDown String


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
update message ((Model ((Env mouse browserWH) as env) state) as model) =
    case message of
        NoOp ->
            ( model, Cmd.none )

        BrowserResized wh ->
            ( Model (Env mouse wh) state, Cmd.none )

        OnMouseDown e ->
            ( Model (Env (Down e e) browserWH) { state | mouseDown = Just ( state.zoom, state.mouseOver ) }
            , Cmd.none
            )

        OnMouseUp _ ->
            case mouse of
                Up ->
                    ( model, Cmd.none )

                Down _ _ ->
                    ( Model (Env Up browserWH) state, Cmd.none )

        OnMouseMove current ->
            case mouse of
                Down start _ ->
                    ( Model (Env (Down start current) browserWH) state, Cmd.none )

                Up ->
                    ( model, Cmd.none )

        MouseOverZoom ->
            ( model, Cmd.none )

        MouseOutZoom ->
            ( model, Cmd.none )

        OnKeyDown key ->
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
            ( Model env { state | zoom = zoom }, Cmd.none )


eventDecoder =
    JD.map2 Tuple.pair IO.timeStampDecoder IO.pageXYDecoder


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions (Model (Env mouse _) _) =
    Sub.batch
        [ IO.onBrowserWH BrowserResized
        , case mouse of
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



-- View


view : Model -> Html Msg
view (Model (Env _ browserWH) state) =
    canvas browserWH
        state.zoom
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
        , SA.fill
            (if isMouseOver then
                "green"

             else
                ""
            )
        ]
    ]
        |> IO.textGroup []


canvas browserWH zoom children =
    IO.canvas browserWH
        [ IO.group [ IO.transform [ IO.scale2 zoom ] ] children ]


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
