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


mapX : (Float -> Float) -> V -> V
mapX =
    Tuple.mapFirst


inc =
    (+) 1


type alias Memory =
    { pos : V
    }


initialMemory : Memory
initialMemory =
    { pos = v -150 0
    }


mapPos : (a -> a) -> { b | pos : a } -> { b | pos : a }
mapPos func model =
    { model | pos = func model.pos }


update : Computer -> Memory -> Memory
update c m =
    mapPos (mapX inc) m


view : Computer -> Memory -> List Shape
view c m =
    [ circle (xin m.pos) 0 10 red ]


main : Game Memory
main =
    game initialMemory update view
