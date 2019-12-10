module GravitronV3.Main exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Range as Range
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
    let
        whiteRect =
            fillRect "white" screen.width screen.height

        blackRect =
            fillRect "black" screen.width screen.height

        fullRect : Shape
        fullRect =
            rect screen.width screen.height

        scales : List Float
        scales =
            Range.init 1 0 |> Range.break 4

        scaledShapes : List Shape
        scaledShapes =
            scales
                |> List.indexedMap
                    (\idx s ->
                        fullRect
                            |> scale s
                            |> stroke
                                (if modBy 2 idx == 0 then
                                    "white"

                                 else
                                    "black"
                                )
                    )
    in
    renderShapes screen
        [ scaledShapes
            |> group
            |> fill "transparent"
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
