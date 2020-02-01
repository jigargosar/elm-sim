module DrawingBlock.Canvas exposing
    ( Transform
    , canvas
    , ellipse
    , fill
    , group
    , polyRect
    , polySquare
    , polygon
    , scale
    , shift
    , transform
    , wrap
    , wrapTransform
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


wrap : List (S.Attribute msg) -> S.Svg msg -> S.Svg msg
wrap attrs =
    List.singleton >> S.g attrs


wrapTransform : List (Transform -> Transform) -> S.Svg msg -> S.Svg msg
wrapTransform list =
    wrap [ transform list ]


canvas : Float2 -> List (S.Attribute msg) -> List (S.Svg msg) -> Html msg
canvas ( w, h ) attrs =
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



--Transform


transform : List (Transform -> Transform) -> S.Attribute msg
transform =
    List.foldl (<|) identity >> toTransformString >> SA.transform


shift : Float2 -> (Transform -> Transform)
shift ( dx, dy ) t =
    { t | x = t.x + dx, y = t.y + dy }


scale : Float -> Transform -> Transform
scale s t =
    { t | scale = s }


type alias Transform =
    { x : Float
    , y : Float
    , scale : Float
    , degrees : Float
    }


identity : Transform
identity =
    Transform 0 0 1 0


toTransformString : { a | x : Float, y : Float, scale : Float, degrees : Float } -> String
toTransformString shape =
    ("translate(" ++ fromFloat shape.x ++ "," ++ fromFloat shape.y ++ ")")
        ++ " "
        ++ ("scale(" ++ fromFloat shape.scale ++ ")")
        ++ " "
        ++ ("rotate(" ++ fromFloat shape.degrees ++ ")")
