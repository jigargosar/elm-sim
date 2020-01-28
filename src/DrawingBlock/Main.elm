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


type alias Model =
    { browserWH : Float2
    , zoom : Float2
    , mouse : Mouse
    , mouseOver : Maybe Element
    , stateOnLastMouseDown : Maybe ( Float2, Maybe Element )
    }


setBrowserWH wh m =
    { m | browserWH = wh }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { browserWH = ( 600, 600 )
      , zoom = ( 1, 1 ) |> N2.scale 2.5
      , mouse = Up
      , mouseOver = Nothing
      , stateOnLastMouseDown = Nothing
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
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        BrowserResized wh ->
            ( setBrowserWH wh model, Cmd.none )

        OnMouseDown e ->
            ( { model | mouse = Down e e, stateOnLastMouseDown = Just ( model.zoom, model.mouseOver ) }, Cmd.none )

        OnMouseUp current ->
            case model.mouse of
                Up ->
                    ( model, Cmd.none )

                Down start _ ->
                    let
                        { dx, dy, isDragging } =
                            eventDiff start current

                        zoom =
                            --if isDnd && sElement == Just ZoomElement then
                            -- N2.scale 1.1 model.zoom
                            --model.zoom |> mapEach (\s -> clamp 0.5 3 (s + (dy / 10)))
                            --
                            --else
                            model.zoom
                    in
                    ( { model | mouse = Up, zoom = zoom }, Cmd.none )

        OnMouseMove current ->
            case model.mouse of
                Down start _ ->
                    let
                        { dx, dy, isDragging } =
                            eventDiff start current

                        zoom =
                            if isDragging then
                                model.zoom |> mapEach (\s -> clamp 0.8 50 (s + dy / 1000))

                            else
                                model.zoom
                    in
                    ( { model | mouse = Down start current, zoom = zoom }, Cmd.none )

                Up ->
                    ( model, Cmd.none )

        MouseOverZoom ->
            ( { model | mouseOver = Just ZoomElement }, Cmd.none )

        MouseOutZoom ->
            if model.mouseOver == Just ZoomElement then
                ( { model | mouseOver = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        OnKeyDown key ->
            let
                _ =
                    Debug.log "key" key

                zoom =
                    case key of
                        "1" ->
                            model.zoom |> mapEach (\s -> clamp 0.05 50 (s + s * 0.1))

                        "2" ->
                            model.zoom |> mapEach (\s -> clamp 0.05 50 (s - s * 0.1))

                        _ ->
                            model.zoom
            in
            ( { model | zoom = zoom }, Cmd.none )


eventDecoder =
    JD.map2 Tuple.pair IO.timeStampDecoder IO.pageXYDecoder


keyDecoder =
    JD.field "key" JD.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ IO.onBrowserWH BrowserResized
        , case model.mouse of
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
view model =
    canvas model
        [ let
            isMouseOverZoom =
                model.mouseOver == Just ZoomElement
          in
          viewZoomData isMouseOverZoom model.zoom
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


canvas model children =
    IO.canvas model.browserWH
        [ IO.group [ IO.transform [ IO.scale2 model.zoom ] ] children ]


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
