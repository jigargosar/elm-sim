module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 as V exposing (..)


type alias Memory =
    { pos : Vec
    , vel : Vec
    }


initialMemory : Memory
initialMemory =
    { pos = vec -150 0
    , vel = vec1
    }


mapPos : (a -> a) -> { b | pos : a } -> { b | pos : a }
mapPos func model =
    { model | pos = func model.pos }


setPos : a -> { b | pos : a } -> { b | pos : a }
setPos =
    mapPos << always


springVec : Vec -> Vec -> Vec
springVec a b =
    minus a b |> scale 0.0001


update : Computer -> Memory -> Memory
update c m =
    let
        mousePos =
            fromRec c.mouse

        vel =
            plus m.vel (springVec m.pos mousePos)

        pos =
            plus vel m.pos
    in
    { m | pos = pos, vel = vel }


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
