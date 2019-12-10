module GravitronV3.Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Dom as Dom
import GravitronV3.Screen as Screen exposing (Screen)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Update.Pipeline exposing (..)


appendWith =
    flip (++)


toPx : (String -> a) -> Float -> a
toPx attr value =
    attr (value |> String.fromFloat |> appendWith "px")



-- Main


type Msg
    = GotViewport Dom.Viewport


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen.initial }
    , Task.perform GotViewport Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotViewport viewport ->
            save { model | screen = Screen.fromViewport viewport }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
