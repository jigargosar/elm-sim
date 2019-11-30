module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Particle as P exposing (Particle)
import GravitronV2.Vector2 as V exposing (..)


type alias Memory =
    Particle


initialMemory : Memory
initialMemory =
    P.initParticle (vec -150 0) vec1


update : Computer -> Memory -> Memory
update c m =
    let
        springPoint =
            fromRec c.mouse

        springConstant =
            0.1

        friction =
            0.9
    in
    m
        |> P.applySpringForceTowardsPoint springPoint springConstant
        |> P.applyFriction friction
        |> P.applyVelocity


view : Computer -> Memory -> List Shape
view c m =
    let
        xy =
            toRec m.position
    in
    [ circle xy.x xy.y 10 red ]


main : Game Memory
main =
    game initialMemory update view
