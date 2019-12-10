module GravitronV3.Canvas exposing (Shape, fill, fillRect, group, rect, renderShapes, scale, stroke, stroke2)

import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Transform as Transform exposing (Transform)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)


type Form
    = Rect Float Float
    | Group (List Shape)


type Brush
    = Fill String
    | Stroke String
    | Stroke2 String Float
    | NoBrush


type Shape
    = Shape Form Brush Transform


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
    Shape (Rect width height) NoBrush Transform.initial


group : List Shape -> Shape
group =
    Group >> toShape


toShape : Form -> Shape
toShape form =
    Shape form NoBrush Transform.initial


fill : String -> Shape -> Shape
fill =
    Fill >> setBrush


setBrush : Brush -> Shape -> Shape
setBrush b (Shape f _ t) =
    Shape f b t


mapTransform : (Transform -> Transform) -> Shape -> Shape
mapTransform func (Shape f b t) =
    Shape f b (func t)


scale : Float -> Shape -> Shape
scale =
    Transform.scale >> mapTransform


renderShapes : Screen -> List Shape -> Html msg
renderShapes screen shapes =
    Screen.toSvg screen (List.map renderShape shapes)


renderShape : Shape -> Svg msg
renderShape (Shape form brush t) =
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
                )
                []

        Group shapes ->
            Svg.g
                (transform (Transform.renderTransform t)
                    :: renderBrush brush
                )
                (List.map renderShape shapes)


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
