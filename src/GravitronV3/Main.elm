module GravitronV3.Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Color exposing (Color)
import GravitronV3.Screen as Screen exposing (Screen)
import Html exposing (Html)
import PointFree exposing (appendBA, appendIf, whenTrue)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import TypedSvg.Types as TT
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
            [ toPx x screen.left
            , toPx y screen.top
            , toPx width screen.width
            , toPx height screen.height
            , fill "#000"
            ]
            []
        ]


type Form
    = Rect Float Float


type Transform
    = Transform Float Float Float Float


type Brush
    = Brush String


type Shape
    = Shape Form Brush Transform


rect : String -> Float -> Float -> Shape
rect color width height =
    Shape (Rect width height) (Brush color) (Transform 0 0 0 1)


renderShape : Shape -> Svg msg
renderShape (Shape form (Brush fillColor) t) =
    let
        f =
            String.fromFloat
    in
    case form of
        Rect w h ->
            Svg.rect
                [ width <| f w
                , height <| f h
                , fill fillColor
                , transform <| renderRectTransform w h t
                ]
                []


renderRectTransform : Float -> Float -> Transform -> String
renderRectTransform w h t =
    let
        f =
            String.fromFloat
    in
    renderTransform t
        ++ (" translate(" ++ f (-w / 2) ++ "," ++ f (-h / 2) ++ ")")


andThenRotateBy : Float -> String -> String
andThenRotateBy angle prefix =
    if angle == 0 then
        prefix

    else
        prefix ++ (" rotate(" ++ String.fromFloat angle ++ ")")


renderTranslate : Float -> Float -> String
renderTranslate x y =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


andThenScaleBy : Float -> String -> String
andThenScaleBy s prefix =
    if s == 1 then
        prefix

    else
        prefix ++ (" scale(" ++ String.fromFloat s ++ ")")


renderTransform : Transform -> String
renderTransform (Transform dx dy angle s) =
    let
        f =
            String.fromFloat
    in
    renderTranslate dx dy
        |> andThenRotateBy angle
        |> andThenScaleBy s


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


timeTravelElement =
    TimeTravel.element Debug.toString Debug.toString TimeTravel.defaultConfig


main =
    timeTravelElement
        --Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
