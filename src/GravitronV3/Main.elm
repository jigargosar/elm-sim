module GravitronV3.Main exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Clock as Clock exposing (Clock)
import GravitronV3.Range as Range
import GravitronV3.Screen as Screen exposing (Screen)
import Html exposing (Html)
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Game


type alias Game =
    {}


initialGame : Game
initialGame =
    {}


updateGame : Game -> Game
updateGame game =
    game


viewGame : Screen -> Game -> Html msg
viewGame screen _ =
    let
        idxToColor idx =
            if modBy 2 idx == 0 then
                "white"

            else
                "black"

        scaledShapes : List Shape
        scaledShapes =
            Range.init 1 0
                |> Range.break 100
                |> List.indexedMap
                    (\idx s ->
                        rect (screen.width * s) (screen.height * s)
                            |> stroke (idxToColor idx |> always "black")
                    )
    in
    renderShapes screen
        [ scaledShapes
            |> group
            |> fill "transparent"
        ]



-- Main


type Msg
    = GotScreen Screen
    | Tick Posix


view : Model -> Html Msg
view { screen, game } =
    viewGame screen game


type alias Model =
    { screen : Screen
    , clock : Clock
    , game : Game
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen.initial
      , clock = Clock.initial
      , game = initialGame
      }
    , Task.perform GotScreen Screen.get
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick posix ->
            save
                { model
                    | clock = Clock.onAnimationFrame posix model.clock
                    , game = updateGame model.game
                }


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
