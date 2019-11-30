module GravitronV2.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Color
import GravitronV2.Render as Render
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Svg exposing (Svg)
import Task
import TypedSvg
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Types exposing (Fill(..), StrokeLinecap(..), StrokeLinejoin(..), Transform(..))



-- Constants
-- Model


type alias Flags =
    ()


type alias Model =
    { mouse : Mouse
    , screen : Screen
    }


type alias Screen =
    { width : Float
    , height : Float
    , left : Float
    , r : Float
    , top : Float
    , b : Float
    }


toScreen : Float -> Float -> Screen
toScreen sw sh =
    let
        scx =
            sw / 2

        scy =
            sh / 2
    in
    { width = sw
    , height = sh
    , left = -scx
    , r = scx
    , top = -scy
    , b = scy
    }


type alias Mouse =
    { x : Float
    , y : Float
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { mouse = Mouse 100 100
      , screen = toScreen 600 600
      }
    , Browser.Dom.getViewport |> Task.perform OnViewport
    )



-- Update


type Msg
    = Tick Float
    | MouseMoved Float Float
    | OnResize Int Int
    | OnViewport Browser.Dom.Viewport


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    , Browser.Events.onResize OnResize
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( -- updateOnTick model
              onTick model
            , Cmd.none
            )

        MouseMoved mx my ->
            let
                mouse =
                    model.mouse

                screen =
                    model.screen
            in
            ( { model | mouse = { mouse | x = mx + screen.left, y = my + screen.top } }
            , Cmd.none
            )

        OnViewport { scene } ->
            ( { model | screen = toScreen scene.width scene.height }
            , Cmd.none
            )

        OnResize width height ->
            ( { model | screen = toScreen (toFloat width) (toFloat height) }
            , Cmd.none
            )


onTick : Model -> Model
onTick model =
    model



-- View


view : Model -> Html Msg
view model =
    let
        screen =
            model.screen

        x =
            screen.left

        y =
            screen.top

        w =
            screen.width

        h =
            screen.height
    in
    Render.fullScreenCanvas x
        y
        w
        h
        Render.black



-- Drawing Helpers


whiteA : Float -> Color.Color
whiteA =
    Color.rgba 1 1 1


fillColor : Color.Color -> Svg.Attribute msg
fillColor =
    Fill >> fill



-- Program


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
