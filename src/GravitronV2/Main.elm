module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)


type V
    = V Float Float


v : Float -> Float -> V
v =
    V


fromRec : { a | x : Float, y : Float } -> V
fromRec { x, y } =
    v x y


type alias XY =
    { x : Float, y : Float }


toRec : V -> XY
toRec (V x y) =
    { x = x, y = y }


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
    setPos (fromRec c.mouse) m


view : Computer -> Memory -> List Shape
view c m =
    let
        xy =
            toRec m.pos
    in
    [ circle xy.x xy.y 10 red ]


main : Game Memory
main =
    game initialMemory update view
