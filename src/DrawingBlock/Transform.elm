module DrawingBlock.Transform exposing (identity, move, render)

import String exposing (fromFloat)
import Svg as S
import Svg.Attributes as SA


render : Transform -> S.Attribute msg
render transformModel =
    SA.transform (toTransformString transformModel)


move : Float -> Float -> Transform -> Transform
move dx dy t =
    { t | x = t.x + dx, y = t.y + dy }


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
