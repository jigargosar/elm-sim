module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)


type alias V =
    ( Float, Float )


v : Float -> Float -> V
v =
    Tuple.pair


xin : V -> Float
xin =
    Tuple.first


type alias Memory =
    { pos : V
    }


initialMemory : Memory
initialMemory =
    { pos = v -150 0
    }


view : Computer -> Memory -> List Shape
view c m =
    [ circle (xin m.pos) 0 10 red ]


main : Game Memory
main =
    game initialMemory view
