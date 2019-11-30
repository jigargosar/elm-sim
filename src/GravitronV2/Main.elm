module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)


type alias V =
    ( Float, Float )


v : Float -> Float -> V
v =
    Tuple.pair


vFromRec : { a | x : Float, y : Float } -> V
vFromRec { x, y } =
    v x y


xin : V -> Float
xin =
    Tuple.first


mapX : (Float -> Float) -> V -> V
mapX =
    Tuple.mapFirst


setX : Float -> V -> V
setX =
    mapX << always


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


setPos : a -> { b | pos : a } -> { b | pos : a }
setPos =
    mapPos << always


update : Computer -> Memory -> Memory
update c m =
    setPos (vFromRec c.mouse) m


view : Computer -> Memory -> List Shape
view c m =
    [ circle (xin m.pos) 0 10 red ]


main : Game Memory
main =
    game initialMemory update view
