module GravitronV5.Circ exposing (..)

import Playground exposing (Number)


type alias Circ a =
    { a
        | x : Number
        , y : Number
        , r : Number
        , vx : Number
        , vy : Number
    }


angleFromTo from to =
    atan2 (to.y - from.y) (to.y - from.y)


shoot : Circ a -> Circ b -> Float -> Circ c -> Circ c
shoot from to speed c =
    let
        ang =
            angleFromTo from to

        offsetCords =
            fromPolar ( from.r + from.r, ang )

        ( x, y ) =
            addCords (cordsOf from) offsetCords

        ( vx, vy ) =
            fromPolar ( speed, ang )
    in
    { c | x = x, y = y, vx = vx, vy = vy }


type alias Cords =
    ( Number, Number )


addCords : Cords -> Cords -> Cords
addCords ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


cordsOf : Circ a -> Cords
cordsOf c =
    ( c.x, c.y )
