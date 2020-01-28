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
import Time



-- Model


type alias Timestamp =
    Float


type alias EventData =
    ( Timestamp, Float2 )


type Mouse
    = Up
    | Down EventData EventData


type MouseOverElement
    = ZoomElement


type alias Model =
    { browserWH : Float2
    , zoom : Float2
    , mouse : Mouse
    , mouseOver : Maybe MouseOverElement
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        BrowserResized wh ->
            ( setBrowserWH wh model, Cmd.none )

        OnMouseDown e ->
            ( { model | mouse = Down e e }, Cmd.none )

        OnMouseUp ( t, xy ) ->
            case model.mouse of
                Up ->
                    ( model, Cmd.none )

                Down ( st, sxy ) _ ->
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

                        isDnd =
                            tooLong || tooFar

                        isClick =
                            not isDnd
                                |> Debug.log "isClick"

                        zoom =
                            --if isDnd && sElement == Just ZoomElement then
                            -- N2.scale 1.1 model.zoom
                            --model.zoom |> mapEach (\s -> clamp 0.5 3 (s + (dy / 10)))
                            --
                            --else
                            model.zoom
                    in
                    ( { model | mouse = Up, zoom = zoom }, Cmd.none )

        OnMouseMove e ->
            case model.mouse of
                Down se _ ->
                    ( { model | mouse = Down se e }, Cmd.none )

                Up ->
                    ( model, Cmd.none )

        MouseOverZoom ->
            ( { model | mouseOver = Just ZoomElement }, Cmd.none )

        MouseOutZoom ->
            if model.mouseOver == Just ZoomElement then
                ( { model | mouseOver = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )


eventDecoder =
    JD.map2 Tuple.pair IO.timeStampDecoder IO.pageXYDecoder


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
