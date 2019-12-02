module GravitronV2.Draw exposing
    ( Color
    , Computer
    , Game
    , Screen
    , Shape
    , circle
    , game
    , green
    , red
    , text
    , white
    , withAlpha
    )

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
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types as TT exposing (AnchorAlignment(..))



-- Model


type alias Flags =
    ()


type alias Computer =
    { mouse : Mouse
    , screen : Screen
    }


type Model memory
    = Model Computer memory


type alias Mouse =
    { x : Float
    , y : Float
    }


init : memory -> Flags -> ( Model memory, Cmd Msg )
init initialMemory _ =
    ( Model
        { mouse = Mouse 0 0
        , screen = screenFromWidthHeight 600 600
        }
        initialMemory
    , Browser.Dom.getViewport |> Task.perform OnViewport
    )



-- Update


type Msg
    = Tick Float
    | MouseMoved Float Float
    | OnResize Int Int
    | OnViewport Browser.Dom.Viewport


subscriptions : Model memory -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    , Browser.Events.onResize OnResize
    ]
        |> Sub.batch


update : Update memory -> Msg -> Model memory -> ( Model memory, Cmd Msg )
update updateMemory message (Model computer memory) =
    case message of
        Tick _ ->
            ( Model computer (updateMemory computer memory)
            , Cmd.none
            )

        MouseMoved mx my ->
            let
                mouse =
                    computer.mouse

                screen =
                    computer.screen
            in
            ( Model { computer | mouse = { mouse | x = mx + screen.left, y = my + screen.top } }
                memory
            , Cmd.none
            )

        OnViewport { scene } ->
            ( Model { computer | screen = screenFromWidthHeight scene.width scene.height }
                memory
            , Cmd.none
            )

        OnResize width height ->
            ( Model { computer | screen = screenFromWidthHeight (toFloat width) (toFloat height) }
                memory
            , Cmd.none
            )



-- View


type Shape
    = Circle Float Float Float Color
    | Text Float Float String


circle : Float -> Float -> Float -> Color -> Shape
circle =
    Circle


text : Float -> Float -> String -> Shape
text =
    Text


renderShapes : Computer -> List Shape -> Html Msg
renderShapes computer shapes =
    let
        screen =
            computer.screen

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
    TA.fill (TT.Fill c)


renderShape shape =
    case shape of
        Circle cx cy r c ->
            Svg.circle [ InPx.cx cx, InPx.cy cy, InPx.r r, fillColor c ] []

        Text x y str ->
            Svg.text_
                [ InPx.x x
                , InPx.y y
                , fillColor white
                , TA.textAnchor TT.AnchorMiddle
                ]
                [ Svg.text str ]



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


green : Color
green =
    Color Color.green


white : Color
white =
    Color Color.white


withAlpha : Float -> Color -> Color
withAlpha alpha (Color c) =
    Color.toRgba c |> setAlphaHelp alpha |> Color.fromRgba |> Color


setAlphaHelp : a -> { b | alpha : a } -> { b | alpha : a }
setAlphaHelp alpha rgba =
    { rgba | alpha = alpha }


render : Float -> Float -> Float -> Float -> Color -> List (Svg msg) -> Html msg
render x y width height color children =
    TypedSvg.svg
        [ Html.Attributes.style "position" "fixed"
        , TA.viewBox x y width height
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


type alias Game memory =
    Program Flags (Model memory) Msg


type alias Update memory =
    Computer -> memory -> memory


type alias View memory =
    Computer -> memory -> List Shape


game : memory -> Update memory -> View memory -> Game memory
game initialMemory updateMemory viewMemory =
    let
        view (Model computer memory) =
            renderShapes computer (viewMemory computer memory)
    in
    Browser.element
        { init = init initialMemory
        , subscriptions = subscriptions
        , update = update updateMemory
        , view = view
        }
