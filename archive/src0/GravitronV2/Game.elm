module GravitronV2.Game exposing
    ( Color
    , Computer
    , GameProgram
    , Keyboard
    , Mouse
    , Screen
    , Shape
    , blue
    , circle
    , circleAt
    , customShape
    , fade
    , freshKeyDown
    , game
    , green
    , group
    , hsl
    , lightRed
    , move
    , noShape
    , purple
    , red
    , scale
    , strokeArc
    , text
    , white
    , withAlpha
    , yellow
    )

import Angle
import Arc2d
import Browser
import Browser.Dom
import Browser.Events
import Color
import Geometry.Svg
import GravitronV2.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as JD
import Point2d
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import TypedSvg
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types as TT exposing (AnchorAlignment(..))



-- Model


type alias Computer =
    { mouse : Mouse
    , screen : Screen
    , keyboard : Keyboard
    }


initialComputer : Computer
initialComputer =
    { mouse = Mouse Vec.zero
    , screen = toScreen 600 600
    , keyboard = initKeyboard
    }


type Game memory
    = Game memory Computer


type alias Mouse =
    { position : Vec
    }


type alias Keyboard =
    { keys : Set String
    , prevKeys : Set String
    }


initKeyboard : Keyboard
initKeyboard =
    { keys = Set.empty
    , prevKeys = Set.empty
    }


freshKeyDown : String -> Computer -> Bool
freshKeyDown key c =
    let
        prev =
            c.keyboard.prevKeys

        curr =
            c.keyboard.keys

        isDown =
            Set.member key
    in
    isDown curr && not (isDown prev)



-- Update


type Msg
    = Tick Float
    | MouseMoved Float Float
    | Resized Int Int
    | GotViewport Browser.Dom.Viewport
    | KeyChanged Bool String


subscriptions : Game memory -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    , Browser.Events.onResize Resized
    , JD.map (KeyChanged True) (JD.field "key" JD.string)
        |> Browser.Events.onKeyDown
    , JD.map (KeyChanged False) (JD.field "key" JD.string)
        |> Browser.Events.onKeyUp
    ]
        |> Sub.batch


gameUpdate : (Computer -> memory -> memory) -> Msg -> Game memory -> Game memory
gameUpdate updateMemory message (Game memory computer) =
    case message of
        Tick _ ->
            Game (updateMemory computer memory)
                { computer
                    | keyboard = setPrevKeys computer.keyboard
                }

        MouseMoved pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top + pageY
            in
            Game memory
                { computer | mouse = mouseMove x y computer.mouse }

        GotViewport { scene } ->
            Game memory { computer | screen = toScreen scene.width scene.height }

        Resized w h ->
            Game memory { computer | screen = toScreen (toFloat w) (toFloat h) }

        KeyChanged isDown key ->
            Game memory
                { computer
                    | keyboard = updateKeyboard isDown key computer.keyboard
                }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | position = vec x y }


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
    { keyboard
        | keys =
            if isDown then
                Set.insert key keyboard.keys

            else
                Set.remove key keyboard.keys
    }


setPrevKeys : Keyboard -> Keyboard
setPrevKeys keyboard =
    { keyboard | prevKeys = keyboard.keys }



-- View


type Form
    = Circle Float Color
    | Group (List Shape)


type Shape
    = Shape Number Number Number Number Number Form
    | Text Float Float String
    | StrokeArc ( Float, Float ) Float ( Float, Float ) Color
    | Custom (Svg Never)
    | NoShape


group : List Shape -> Shape
group l =
    Group l |> toShape


circleAt : Float -> Float -> Float -> Color -> Shape
circleAt x y r c =
    circle r c |> move x y


move : Number -> Number -> Shape -> Shape
move dx dy =
    mapXY ((+) dx) ((+) dy)


scale : Number -> Shape -> Shape
scale ns =
    mapScale ((*) ns)


fade : Number -> Shape -> Shape
fade o shape =
    case shape of
        Shape x y a s _ f ->
            Shape x y a s o f

        _ ->
            shape


mapXY : (Number -> Number) -> (Number -> Number) -> Shape -> Shape
mapXY funcX funcY shape =
    case shape of
        Shape x y a s o f ->
            Shape (funcX x) (funcY y) a s o f

        _ ->
            shape


mapScale : (Number -> Number) -> Shape -> Shape
mapScale func shape =
    case shape of
        Shape x y a s o f ->
            Shape x y a (func s) o f

        _ ->
            shape


circle : Float -> Color -> Shape
circle r c =
    Circle r c
        |> toShape


toShape : Form -> Shape
toShape =
    Shape 0 0 0 1 1


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


render : Screen -> List Shape -> Html Msg
render screen shapes =
    let
        x =
            screen.left

        y =
            screen.top

        w =
            screen.width

        h =
            screen.height
    in
    TypedSvg.svg
        [ Html.Attributes.style "position" "fixed"
        , TA.viewBox x y w h
        , InPx.width w
        , InPx.height h
        ]
        (TypedSvg.rect
            [ InPx.x x
            , InPx.y y
            , InPx.width w
            , InPx.height h
            , fillColor black
            ]
            []
            :: List.map renderShape shapes
        )


fillColor : Color -> Svg.Attribute msg
fillColor (Color c) =
    TA.fill (TT.Fill c)


fillNone : Svg.Attribute msg
fillNone =
    TA.fill TT.FillNone


strokeColor : Color -> Svg.Attribute msg
strokeColor (Color c) =
    TA.stroke c


type alias Number =
    Float


renderTransform : Number -> Number -> Number -> Number -> String
renderTransform x y_ a_ s =
    let
        y =
            -y_

        a =
            -a_
    in
    if a == 0 then
        if s == 1 then
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ")"

        else
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") scale(" ++ String.fromFloat s ++ ")"

    else if s == 1 then
        "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromFloat -a ++ ")"

    else
        "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ ") rotate(" ++ String.fromFloat -a ++ ") scale(" ++ String.fromFloat s ++ ")"


renderAlpha : Number -> List (Svg.Attribute msg)
renderAlpha alpha =
    if alpha == 1 then
        []

    else
        [ SA.opacity (String.fromFloat (clamp 0 1 alpha)) ]


renderShape : Shape -> Svg msg
renderShape shape =
    case shape of
        Shape x y angle s alpha form ->
            case form of
                Circle radius color ->
                    Svg.circle
                        (InPx.r radius
                            :: fillColor color
                            :: SA.transform (renderTransform x y angle s)
                            :: renderAlpha alpha
                        )
                        []

                Group list ->
                    Svg.g
                        (SA.transform (renderTransform x y angle s)
                            :: renderAlpha alpha
                        )
                        (List.map renderShape list)

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
                , InPx.strokeWidth 2
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


toScreen : Float -> Float -> Screen
toScreen width height =
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


blue : Color
blue =
    Color Color.blue


yellow : Color
yellow =
    Color Color.yellow


lightRed : Color
lightRed =
    Color Color.lightRed


hsl : Float -> Float -> Float -> Color
hsl h s l =
    Color.hsl h s l
        |> Color


green : Color
green =
    Color Color.green


purple : Color
purple =
    Color Color.purple


white : Color
white =
    Color Color.white


withAlpha : Float -> Color -> Color
withAlpha alpha (Color c) =
    Color.toRgba c |> setAlphaHelp alpha |> Color.fromRgba |> Color


setAlphaHelp : a -> { b | alpha : a } -> { b | alpha : a }
setAlphaHelp alpha rgba =
    { rgba | alpha = alpha }


type alias GameProgram memory =
    Program () (Game memory) Msg


game :
    memory
    -> (Computer -> memory -> memory)
    -> (Computer -> memory -> List Shape)
    -> GameProgram memory
game initialMemory updateMemory viewMemory =
    let
        view : Game memory -> Html Msg
        view (Game memory computer) =
            render computer.screen (viewMemory computer memory)

        init () =
            ( Game initialMemory initialComputer
            , Browser.Dom.getViewport |> Task.perform GotViewport
            )

        update msg model =
            ( gameUpdate updateMemory msg model, Cmd.none )
    in
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
