module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html, text)
import Html.Attributes as H
import Random exposing (Seed)
import Task
import TypedSvg exposing (..)
import TypedSvg.Attributes as A exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as T exposing (Transform(..), percent)



-- Screen


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



-- Transform


type alias Transforms =
    { x : Float
    , y : Float
    }


noTransform : Transforms
noTransform =
    { x = 0
    , y = 0
    }


move : Float -> Float -> Transforms -> Transforms
move dx dy ({ x, y } as t) =
    { t | x = x + dx, y = y + dy }



-- Model


type alias Flags =
    { now : Int
    }


type alias Model =
    { screen : Screen
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { screen = screenFromWH 600 400
      , seed = Random.initialSeed flags.now
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


view : Model -> Html msg
view model =
    let
        circleT =
            noTransform
                |> move screen.l 0

        screen =
            model.screen
    in
    render screen [ renderCircle 10 circleT ]


render : Screen -> List (Svg msg) -> Html msg
render screen =
    let
        w =
            screen.w

        h =
            screen.h

        x =
            screen.l

        y =
            screen.b
    in
    svg
        [ viewBox x y w h
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , A.width (percent 100)
        , A.height (percent 100)
        ]


renderCircle : Float -> Transforms -> Svg msg
renderCircle radius transforms_ =
    -- renderCircle color radius x y angle s alpha =
    circle
        (r radius
            --:: fill (renderColor color)
            :: transform (renderTransforms transforms_)
            --:: renderAlpha alpha
            :: []
        )
        []


renderTransforms : Transforms -> List T.Transform
renderTransforms { x, y } =
    let
        a =
            0
    in
    [ Translate x -y, Rotate -a 0 0 ]
