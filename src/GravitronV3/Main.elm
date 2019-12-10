module GravitronV3.Main exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Draw exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import Html exposing (Html)
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Main


type Msg
    = GotScreen Screen
    | Tick Posix


view : Model -> Html Msg
view { screen } =
    renderShapes screen
        [ rect "#000" screen.width screen.height
        , rect "#fff" (screen.width / 2) (screen.height / 2)
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



--noinspection ElmUnusedSymbol


timeTravelElement =
    TimeTravel.element Debug.toString Debug.toString TimeTravel.defaultConfig


main =
    --timeTravelElement
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
