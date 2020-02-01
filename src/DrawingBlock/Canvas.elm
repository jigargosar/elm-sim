module DrawingBlock.Canvas exposing
    ( canvas
    , ellipse
    , fill
    , group
    , group1
    , polyRect
    , polySquare
    , polygon
    )

import Html exposing (Html)
import Number2 exposing (Float2)
import String exposing (fromFloat)
import Svg as S
import Svg.Attributes as SA


fill : String.String -> S.Attribute msg
fill =
    SA.fill


polyRect : Float2 -> List (S.Attribute msg) -> S.Svg msg
polyRect wh =
    polygon (rectToPolygonPoints wh)


polySquare : Float -> List (S.Attribute msg) -> S.Svg msg
polySquare w =
    polyRect ( w, w )


rectToPolygonPoints : Float2 -> List ( Float, Float )
rectToPolygonPoints ( w, h ) =
    let
        pt sx sy =
            ( w / 2 * sx, h / 2 * sy )
    in
    [ pt -1 -1, pt -1 1, pt 1 1, pt 1 -1 ]


polygon : List ( Float, Float ) -> List (S.Attribute msg) -> S.Svg msg
polygon points attrs =
    S.polygon
        (SA.points (List.foldl addPoint "" points)
            :: attrs
        )
        []


addPoint : ( Float, Float ) -> String -> String
addPoint ( x, y ) str =
    str ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


ellipse : Float2 -> List (S.Attribute msg) -> S.Svg msg
ellipse ( w, h ) attrs =
    S.ellipse
        (SA.rx (fromFloat w)
            :: SA.ry (fromFloat h)
            :: attrs
        )
        []


group : List (S.Attribute msg) -> List (S.Svg msg) -> S.Svg msg
group =
    S.g


group1 : List (S.Attribute msg) -> S.Svg msg -> S.Svg msg
group1 attrs =
    List.singleton >> S.g attrs


canvas : Float -> Float -> List (S.Attribute msg) -> List (S.Svg msg) -> Html msg
canvas w h attrs =
    S.svg
        (SA.viewBox (fromFloat (-w / 2) ++ " " ++ fromFloat (-h / 2) ++ " " ++ fromFloat w ++ " " ++ fromFloat h)
            :: SA.width "100%"
            :: SA.height "100%"
            :: SA.style
                """
                    left : 0;
                    top : 0;
                    position : fixed;
                """
            :: attrs
        )
