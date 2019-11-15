module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as H
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg.Keyed
import Svg.Lazy
import Task
import Time exposing (Posix)
import TypedSvg exposing (..)
import TypedSvg.Attributes as A exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Transform(..), percent)



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



-- Model


type alias Flags =
    { now : Int
    }


type alias Walker =
    ( Int, Int )


type alias Model =
    { screen : Screen
    , seed : Seed
    , walker : Walker
    , walkerHistory : Set Walker
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        walker =
            ( 0, 0 )
    in
    ( { screen = screenFromWH 600 400
      , seed = Random.initialSeed flags.now
      , walker = walker
      , walkerHistory = Set.singleton walker
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )


type Msg
    = NoOp
    | GotViewport Viewport
    | OnBrowserResize Int Int
    | OnAnimationFrame Posix


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnBrowserResize
        , Browser.Events.onAnimationFrame OnAnimationFrame
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screen = screenFromWH scene.width scene.height }, Cmd.none )

        OnBrowserResize w h ->
            ( { model | screen = screenFromWH (toFloat w) (toFloat h) }, Cmd.none )

        OnAnimationFrame posix ->
            ( updateGame posix model, Cmd.none )


updateGame : Posix -> Model -> Model
updateGame _ model =
    randomUpdateWalker model
        |> updateWalkerHistory


updateWalkerHistory : Model -> Model
updateWalkerHistory model =
    let
        ( x, y ) =
            model.walker
    in
    { model | walkerHistory = Set.insert ( x, y ) model.walkerHistory }


randomUpdateWalker : Model -> Model
randomUpdateWalker =
    randomUpdateWalkerHelp 10


randomUpdateWalkerHelp : Int -> Model -> Model
randomUpdateWalkerHelp maxTries model =
    let
        ( walker, seed ) =
            Random.step (updateWalkerGenerator model.walker) model.seed
    in
    if maxTries > 0 && Set.member walker model.walkerHistory then
        randomUpdateWalkerHelp (maxTries - 1) { model | seed = seed }

    else
        { model | walker = walker, seed = seed }


updateWalkerGenerator : Walker -> Generator Walker
updateWalkerGenerator ( x, y ) =
    Random.pair (Random.int -1 1) (Random.int -1 1)
        |> Random.map
            (\( dx, dy ) ->
                ( x + dx, y + dy )
            )


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
        screen =
            model.screen
    in
    render screen [ Svg.Keyed.node "g" [] (Set.toList model.walkerHistory |> List.map renderBit) ]


renderBit : Walker -> ( String, Svg msg )
renderBit ( x, y ) =
    ( String.fromInt x ++ "," ++ String.fromInt y, Svg.Lazy.lazy2 renderBitAt x y )


renderBitAt : Int -> Int -> Svg msg
renderBitAt x y =
    circle
        [ r 2
        , transform [ Translate (toFloat x) (toFloat y) ]
        ]
        []


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
