module GravitronV2.Draw exposing
    ( Color
    , Computer
    , Game
    , Screen
    , Shape
    , circle
    , customShape
    , game
    , green
    , noShape
    , red
    , strokeArc
    , text
    , white
    , withAlpha
    )

import Angle
import Arc2d
import Browser
import Browser.Dom
import Browser.Events
import Color
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Point2d
import Set exposing (Set)
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
    , keyboard : Keyboard
    }


type Model memory
    = Model Computer memory


type alias Mouse =
    { x : Float
    , y : Float
    }


type alias Keyboard =
    { keys : Set String
    }


initKeyboard : Keyboard
initKeyboard =
    { keys = Set.empty }


init : memory -> Flags -> ( Model memory, Cmd Msg )
init initialMemory _ =
    ( Model
        { mouse = Mouse 0 0
        , screen = screenFromWidthHeight 600 600
        , keyboard = initKeyboard
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
    | OnKeyDown String
    | OnKeyUp String


subscriptions : Model memory -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    , Browser.Events.onResize OnResize
    , JD.map OnKeyDown (JD.field "key" JD.string)
        |> Browser.Events.onKeyDown
    , JD.map OnKeyUp (JD.field "key" JD.string)
        |> Browser.Events.onKeyUp
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

        OnKeyDown key ->
            ( Model
                { computer
                    | keyboard = onKeyDown key computer.keyboard
                }
                memory
            , Cmd.none
            )

        OnKeyUp key ->
            ( Model
                { computer
                    | keyboard = onKeyUp key computer.keyboard
                }
                memory
            , Cmd.none
            )


onKeyDown : String -> Keyboard -> Keyboard
onKeyDown key keyboard =
    { keyboard | keys = Set.insert key keyboard.keys }


onKeyUp : String -> Keyboard -> Keyboard
onKeyUp key keyboard =
    { keyboard | keys = Set.remove key keyboard.keys }



-- View


type Shape
    = Circle Float Float Float Color
    | Text Float Float String
    | StrokeArc ( Float, Float ) Float ( Float, Float ) Color
    | Custom (Svg Never)
    | NoShape


circle : Float -> Float -> Float -> Color -> Shape
circle =
    Circle


noShape : Shape
noShape =
    NoShape


text : Float -> Float -> String -> Shape
text =
    Text


strokeArc : ( Float, Float ) -> Float -> ( Float, Float ) -> Color -> Shape
strokeArc =
    StrokeArc


customShape : Svg Never -> Shape
customShape =
    Custom


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


fillNone : Svg.Attribute msg
fillNone =
    TA.fill TT.FillNone


strokeColor : Color -> Svg.Attribute msg
strokeColor (Color c) =
    TA.stroke c


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

        Custom custom ->
            custom |> Svg.map never

        StrokeArc ( cx, cy ) angle ( sx, sy ) c ->
            Geometry.Svg.arc2d
                [ strokeColor c
                , fillNone
                ]
                (Arc2d.sweptAround (Point2d.unitless cx cy)
                    (Angle.radians angle)
                    (Point2d.unitless sx sy)
                )

        NoShape ->
            Svg.text ""



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
