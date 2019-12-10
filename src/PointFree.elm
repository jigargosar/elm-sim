module PointFree exposing (..)

import Basics.Extra exposing (flip)


with : (a -> b) -> (b -> a -> c) -> a -> c
with func1 func2 model =
    func2 (func1 model) model


when : (a -> Bool) -> (a -> a) -> a -> a
when p t v =
    if p v then
        t v

    else
        v


unless : (a -> Bool) -> (a -> a) -> a -> a
unless p =
    when (p >> not)


dec : number -> number
dec =
    (+) -1


clamp0 : number -> number -> number
clamp0 =
    clamp 0


appendAB : appendable -> appendable -> appendable
appendAB =
    (++)


appendBA : appendable -> appendable -> appendable
appendBA =
    flip appendAB


pairedTo : b -> a -> ( a, b )
pairedTo =
    flip Tuple.pair


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach func =
    Tuple.mapBoth func func


subAB : number -> number -> number
subAB =
    (-)


subBA : number -> number -> number
subBA =
    flip subAB


mod : Int -> Int -> Int
mod =
    flip modBy
