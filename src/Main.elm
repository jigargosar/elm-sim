module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html, text)
import Html.Attributes as H
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


type alias Screen =
    { w : Float
    , h : Float
    , t : Float
    , b : Float
    , l : Float
    , r : Float
    }


screenFromWH : Float -> Float -> Screen
screenFromWH w h =
    { w = w
    , h = h
    , t = h / 2 * -1
    , b = h / 2 * -1
    , l = w / 2 * -1
    , r = w / 2
    }


type alias Flags =
    {}


type alias Model =
    { screen : Screen }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screen = screenFromWH 600 400
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )


type Msg
    = NoOp
    | GotViewport Viewport
    | OnBrowserResize Int Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize OnBrowserResize


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screen = screenFromWH scene.width scene.height }, Cmd.none )

        OnBrowserResize w h ->
            ( { model | screen = screenFromWH (toFloat w) (toFloat h) }, Cmd.none )


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


view : Model -> Html msg
view model =
    render model.screen [ renderCircle 10 ]


render : Screen -> List (Svg msg) -> Html msg
render screen =
    let
        w =
            String.fromFloat screen.w

        h =
            String.fromFloat screen.h

        x =
            String.fromFloat screen.l

        y =
            String.fromFloat screen.b
    in
    svg
        [ viewBox (x ++ " " ++ y ++ " " ++ w ++ " " ++ h)
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%"
        ]


renderCircle : Float -> Svg msg
renderCircle radius =
    -- renderCircle color radius x y angle s alpha =
    Svg.circle
        (r (String.fromFloat radius)
            --:: fill (renderColor color)
            --:: transform (renderTransform x y angle s)
            --:: renderAlpha alpha
            :: []
        )
        []
