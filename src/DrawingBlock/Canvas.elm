module DrawingBlock.Canvas exposing
    ( Transform
    , canvas
    , ellipse
    , group
    , identityTransform
    , move
    , polyRect
    , polygon
    , transform
    )

import Html exposing (Html)
import String exposing (fromFloat)
import Svg as S
import Svg.Attributes as SA


transform : Transform -> S.Attribute msg
transform transformModel =
    SA.transform (toTransformString transformModel)


move : Float -> Float -> Transform -> Transform
move dx dy t =
    { t | x = t.x + dx, y = t.y + dy }


polyRect : Float -> Float -> List (S.Attribute msg) -> S.Svg msg
polyRect w h =
    polygon (rectToPolygonPoints w h)


rectToPolygonPoints : Float -> Float -> List ( Float, Float )
rectToPolygonPoints w h =
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


ellipse : Float -> Float -> List (S.Attribute msg) -> S.Svg msg
ellipse w h attrs =
    S.ellipse
        (SA.rx (fromFloat w)
            :: SA.ry (fromFloat h)
            :: attrs
        )
        []


group : List (S.Attribute msg) -> List (S.Svg msg) -> S.Svg msg
group =
    S.g


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


type alias Transform =
    { x : Float
    , y : Float
    , scale : Float
    , degrees : Float
    }


identityTransform : Transform
identityTransform =
    Transform 0 0 1 0


addPoint : ( Float, Float ) -> String -> String
addPoint ( x, y ) str =
    str ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


toTransformString : { a | x : Float, y : Float, scale : Float, degrees : Float } -> String
toTransformString shape =
    ("translate(" ++ fromFloat shape.x ++ "," ++ fromFloat shape.y ++ ")")
        ++ " "
        ++ ("scale(" ++ fromFloat shape.scale ++ ")")
        ++ " "
        ++ ("rotate(" ++ fromFloat shape.degrees ++ ")")
