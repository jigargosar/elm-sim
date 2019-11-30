module GravitronV2.Draw exposing (Color, Screen, black, fullScreenCanvas, main, screenFromWidthHeight)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Svg exposing (Svg)
import Task
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types



-- Model


type alias Flags =
    ()


type alias Model =
    { mouse : Mouse
    , screen : Screen
    }


type alias Mouse =
    { x : Float
    , y : Float
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { mouse = Mouse 100 100
      , screen = screenFromWidthHeight 600 600
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
            ( onTick model
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
            ( { model | screen = screenFromWidthHeight scene.width scene.height }
            , Cmd.none
            )

        OnResize width height ->
            ( { model | screen = screenFromWidthHeight (toFloat width) (toFloat height) }
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
    fullScreenCanvas x
        y
        w
        h
        black
        []



-- Program


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Screen =
    { left : Float
    , right : Float
    , top : Float
    , bottom : Float
    , width : Float
    , height : Float
    }


screenFromWidthHeight : Float -> Float -> Screen
screenFromWidthHeight width height =
    let
        halfWidth =
            width / 2

        halfHeight =
            height / 2
    in
    { left = -halfWidth
    , right = halfWidth
    , top = -halfHeight
    , bottom = halfHeight
    , width = width
    , height = height
    }


type Color
    = Color Color.Color


black : Color
black =
    Color Color.black


fullScreenCanvas : Float -> Float -> Float -> Float -> Color -> List (Svg msg) -> Html msg
fullScreenCanvas x y width height (Color fillColor) children =
    TypedSvg.svg
        [ Html.Attributes.style "position" "fixed"
        , TypedSvg.Attributes.viewBox x y width height
        , InPx.width width
        , InPx.height height
        ]
        (TypedSvg.rect
            [ InPx.x x
            , InPx.y y
            , InPx.width width
            , InPx.height height
            , TypedSvg.Attributes.fill (TypedSvg.Types.Fill fillColor)
            ]
            []
            :: children
        )
