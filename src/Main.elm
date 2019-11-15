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
import TypedSvg.Core as Svg exposing (Svg)
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
    , walkerHistorySet : Set Walker
    , walkerHistoryList : List Walker
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
      , walkerHistorySet = Set.singleton walker
      , walkerHistoryList = List.singleton walker
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
updateGame _ =
    walkOnce >> walkOnce >> walkOnce >> walkOnce >> walkOnce >> walkOnce


walkOnce =
    randomUpdateWalker >> updateWalkerHistory


updateWalkerHistory : Model -> Model
updateWalkerHistory model =
    { model
        | walkerHistorySet = Set.insert model.walker model.walkerHistorySet
        , walkerHistoryList = model.walker :: model.walkerHistoryList
    }


randomUpdateWalker : Model -> Model
randomUpdateWalker =
    randomUpdateWalkerHelp 10


randomUpdateWalkerHelp : Int -> Model -> Model
randomUpdateWalkerHelp maxTries model =
    let
        ( walker, seed ) =
            Random.step (updateWalkerGenerator model.walker) model.seed
    in
    if maxTries > 0 && Set.member walker model.walkerHistorySet then
        randomUpdateWalkerHelp (maxTries - 1) { model | seed = seed }

    else
        { model | walker = walker, seed = seed }


randomIntRange : Int -> Generator Int
randomIntRange n =
    Random.int -n n


double : a -> ( a, a )
double n =
    ( n, n )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry func ( a, b ) =
    func a b


tupleLift : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
tupleLift f ( a, b ) ( c, d ) =
    ( f a c, f b d )


updateWalkerGenerator : Walker -> Generator Walker
updateWalkerGenerator walker =
    double (randomIntRange 1)
        |> uncurry Random.pair
        |> Random.map (tupleLift (+) walker)


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

        list =
            List.reverse model.walkerHistoryList
    in
    render screen
        [ renderKeyedGroup [] renderKeyedBit (List.reverse model.walkerHistoryList)

        {- ,
           renderGroup [] renderBit list
        -}
        ]


renderKeyedGroup : List (Svg.Attribute msg) -> (a -> ( String, Svg msg )) -> List a -> Svg msg
renderKeyedGroup attrs func list =
    Svg.Keyed.node "g" attrs (List.map func list)


renderGroup : List (Svg.Attribute msg) -> (a -> Svg msg) -> List a -> Svg msg
renderGroup attrs func list =
    g attrs (List.map func list)


renderKeyedBit : Walker -> ( String, Svg msg )
renderKeyedBit ( x, y ) =
    ( String.fromInt x ++ "," ++ String.fromInt y, renderBit ( x, y ) )


renderBit : ( Int, Int ) -> Svg msg
renderBit ( x, y ) =
    Svg.Lazy.lazy2 renderBitAt x y


renderBitAt : Int -> Int -> Svg msg
renderBitAt x y =
    circle
        [ r 0.5
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
