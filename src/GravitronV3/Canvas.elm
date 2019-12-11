module GravitronV3.Canvas exposing (Shape, circle, fade, fill, fillRect, group, move, rect, renderShapes, scale, stroke, stroke2)

import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Transform as Transform exposing (Transform)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)


type Form
    = Rect Float Float
    | Group (List Shape)
    | Circle Float


type Brush
    = Fill String
    | Stroke String
    | Stroke2 String Float
    | NoBrush


type Shape
    = Shape Form Brush Float Transform


fillRect : String -> Float -> Float -> Shape
fillRect color w h =
    rect w h |> fill color


stroke : String -> Shape -> Shape
stroke c =
    setBrush (Stroke c)


stroke2 : String -> Float -> Shape -> Shape
stroke2 c s =
    setBrush (Stroke2 c s)


rect : Float -> Float -> Shape
rect width height =
    toShape (Rect width height)


circle : Float -> Shape
circle =
    Circle >> toShape


group : List Shape -> Shape
group =
    Group >> toShape


toShape : Form -> Shape
toShape form =
    Shape form NoBrush 1 Transform.initial


fill : String -> Shape -> Shape
fill =
    Fill >> setBrush


setBrush : Brush -> Shape -> Shape
setBrush b (Shape f _ o t) =
    Shape f b o t


mapTransform : (Transform -> Transform) -> Shape -> Shape
mapTransform func (Shape f b o t) =
    Shape f b o (func t)


scale : Float -> Shape -> Shape
scale =
    Transform.scale >> mapTransform


fade : Float -> Shape -> Shape
fade o (Shape f b _ t) =
    Shape f b o t


move : ( Float, Float ) -> Shape -> Shape
move ( dx, dy ) =
    Transform.move dx dy |> mapTransform


renderShapes : Screen -> List Shape -> Html msg
renderShapes screen shapes =
    Screen.toSvg screen (List.map renderShape shapes)


renderShape : Shape -> Svg msg
renderShape (Shape form brush alpha t) =
    let
        f =
            String.fromFloat
    in
    case form of
        Rect w h ->
            Svg.rect
                ([ width <| f w
                 , height <| f h
                 , transform <| Transform.renderRectTransform w h t
                 ]
                    ++ renderBrush brush
                    ++ renderAlpha alpha
                )
                []

        Group shapes ->
            Svg.g
                (transform (Transform.renderTransform t)
                    :: renderBrush brush
                    ++ renderAlpha alpha
                )
                (List.map renderShape shapes)

        Circle radius ->
            Svg.circle
                ([ r <| f radius
                 , transform <| Transform.renderTransform t
                 ]
                    ++ renderBrush brush
                    ++ renderAlpha alpha
                )
                []


renderAlpha : Float -> List (Svg.Attribute msg)
renderAlpha alpha =
    if alpha == 1 then
        []

    else
        [ opacity (String.fromFloat (clamp 0 1 alpha)) ]


renderBrush : Brush -> List (Attribute msg)
renderBrush brush =
    case brush of
        NoBrush ->
            []

        Fill fc ->
            [ S.fill fc ]

        Stroke sc ->
            [ S.stroke sc ]

        Stroke2 sc sw ->
            [ S.stroke sc, S.strokeWidth <| String.fromFloat sw ]
