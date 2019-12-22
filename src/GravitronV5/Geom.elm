module GravitronV5.Geom exposing (..)

import Playground exposing (..)


springToMouse : { a | x : Float, y : Float } -> { b | x : Float, y : Float, vx : Float, vy : Float } -> { b | x : Float, y : Float, vx : Float, vy : Float }
springToMouse mouse ({ x, y, vx, vy } as p) =
    let
        k =
            0.2

        f =
            0.5

        ( dx, dy ) =
            ( (mouse.x - x) * k, (mouse.y - y) * k )
    in
    { p | vx = (vx + dx) * f, vy = (vy + dy) * f }


addVelToPos : { a | x : number, vx : number, y : number, vy : number } -> { a | x : number, vx : number, y : number, vy : number }
addVelToPos b =
    { b | x = b.x + b.vx, y = b.y + b.vy }


gravitateVelTo :
    Float
    -> Float
    -> { a | x : Float, y : Float, vx : Float, vy : Float }
    -> { a | x : Float, y : Float, vx : Float, vy : Float }
gravitateVelTo tx ty b =
    let
        ( dx, dy ) =
            ( tx - b.x, ty - b.y )
                |> toPolar
                |> Tuple.mapFirst (\m -> 20 / m)
                |> fromPolar
    in
    { b | vx = b.vx + dx, vy = b.vy + dy }


bounceVel : Float -> Screen -> { a | x : Float, y : Float, vx : Float, vy : Float } -> { a | x : Float, y : Float, vx : Float, vy : Float }
bounceVel bounceFactor screen ({ x, y, vx, vy } as b) =
    let
        ( nvx, nvy ) =
            newBounceVelInScreen bounceFactor screen x y vx vy
    in
    { b | vx = nvx, vy = nvy }


newBounceVelInScreen :
    Float
    -> Screen
    -> Float
    -> Float
    -> Float
    -> Float
    -> ( Float, Float )
newBounceVelInScreen bounceFriction scr x y vx vy =
    let
        nvx =
            if
                (x < scr.left && vx < 0)
                    || (x > scr.right && vx > 0)
            then
                negate vx

            else
                vx

        nvy =
            if
                (y < scr.bottom && vy < 0)
                    || (y > scr.top && vy > 0)
            then
                negate vy

            else
                vy
    in
    if nvx /= vx || nvy /= vy then
        toPolar ( nvx, nvy )
            |> Tuple.mapFirst ((*) bounceFriction)
            |> fromPolar

    else
        ( nvx, nvy )


ccc : Number -> Number -> Number -> Number -> Number -> Number -> Bool
ccc x y r x2 y2 r2 =
    ((x2 - x) ^ 2 + (y2 - y) ^ 2)
        < (r ^ 2 + r2 ^ 2)


angleFromTo : Float -> Float -> Float -> Float -> Float
angleFromTo x y x2 y2 =
    atan2 (y2 - y) (x2 - x)


angleFromToRec : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Float
angleFromToRec a b =
    angleFromTo a.x a.y b.x b.y


applyVel : { a | x : number, y : number, vx : number, vy : number } -> { a | x : number, y : number, vx : number, vy : number }
applyVel e =
    let
        { x, y, vx, vy } =
            e
    in
    { e | x = x + vx, y = y + vy }


addBoth : ( number, number ) -> ( number, number ) -> ( number, number )
addBoth ( x, y ) ( x2, y2 ) =
    ( x + x2, y + y2 )
