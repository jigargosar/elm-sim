module GravitronV5.Circ exposing (Circ, shoot)

import Playground exposing (Number)


type alias Circ a =
    { a
        | x : Number
        , y : Number
        , r : Number
        , vx : Number
        , vy : Number
    }


shoot : Circ a -> Circ b -> Float -> Circ c -> Circ c
shoot from to speed c =
    let
        ang =
            angleFromTo from to

        offsetCords =
            fromPolar ( from.r + from.r, ang )

        cords =
            addCords (cordsOf from) offsetCords

        ( vx, vy ) =
            fromPolar ( speed, ang )
    in
    { c | vx = vx, vy = vy }
        |> setCords cords


type alias Cords =
    ( Number, Number )


addCords : Cords -> Cords -> Cords
addCords ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


cordsOf : Circ a -> Cords
cordsOf c =
    ( c.x, c.y )


setCords : Cords -> Circ a -> Circ a
setCords ( x, y ) c =
    { c | x = x, y = y }


setSpeedAngle : Cords -> Circ a -> Circ a
setSpeedAngle polar c =
    let
        ( vx, vy ) =
            fromPolar polar
    in
    { c | vx = vx, vy = vy }


angleFromTo : Circ a -> Circ b -> Float
angleFromTo from to =
    angleFromToCords (cordsOf from) (cordsOf to)


angleFromToCords : Cords -> Cords -> Float
angleFromToCords ( x1, y1 ) ( x2, y2 ) =
    atan2 (y2 - y1) (x2 - x1)
