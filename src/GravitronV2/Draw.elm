module GravitronV2.Draw exposing (Picture, circle, picture, red)

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


type Shape
    = Circle Float Float Float Color


circle : Float -> Float -> Float -> Color -> Shape
circle =
    Circle


renderShapes : List Shape -> Model -> Html Msg
renderShapes shapes model =
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
    render x
        y
        w
        h
        black
        (List.map renderShape shapes)


fillColor : Color -> Svg.Attribute msg
fillColor (Color c) =
    TypedSvg.Attributes.fill (TypedSvg.Types.Fill c)


renderShape shape =
    case shape of
        Circle cx cy r c ->
            Svg.circle [ InPx.cx cx, InPx.cy cy, InPx.r r, fillColor c ] []



-- Program


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


red : Color
red =
    Color Color.red


render : Float -> Float -> Float -> Float -> Color -> List (Svg msg) -> Html msg
render x y width height color children =
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
            , fillColor color
            ]
            []
            :: children
        )


type alias Picture =
    Program Flags Model Msg


picture : List Shape -> Picture
picture shapes =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = renderShapes shapes
        }
