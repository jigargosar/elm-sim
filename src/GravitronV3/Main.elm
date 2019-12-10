module GravitronV3.Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Dom as Dom
import Browser.Events as E
import GravitronV3.Screen as Screen exposing (Screen)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Posix)
import Update.Pipeline exposing (..)


appendWith =
    flip (++)


toPx : (String -> a) -> Float -> a
toPx attr value =
    attr (value |> String.fromFloat |> appendWith "px")



-- Main


type Msg
    = GotScreen Screen
    | Tick Posix


view : Model -> Html Msg
view { screen } =
    Screen.toSvg screen
        [ Svg.rect
            [ toPx x 0
            , toPx y 0
            , width "100%"
            , height "100%"
            , fill "#000"
            ]
            []
        ]


type alias Model =
    { screen : Screen
    , animationClock : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen.initial
      , animationClock = Time.millisToPosix 0
      }
    , Task.perform GotScreen Screen.get
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick animationClock ->
            save { model | animationClock = animationClock }


subscriptions _ =
    Sub.batch
        [ Screen.onResize GotScreen
        , E.onAnimationFrame Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
