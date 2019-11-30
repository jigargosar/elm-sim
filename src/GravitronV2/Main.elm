module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 as V exposing (..)


type alias Memory =
    Particle


initialMemory : Memory
initialMemory =
    initParticle (vec -150 0) vec1 0.9


initParticle : Vec -> Vec -> Float -> Particle
initParticle =
    Particle


type alias Particle =
    { pos : Vec
    , vel : Vec
    , friction : Float
    }


springTo : Vec -> Float -> Particle -> Particle
springTo springPoint k model =
    { model
        | vel =
            springForceFrom model.pos springPoint k
                |> integrate model.vel
    }


applyFriction : Particle -> Particle
applyFriction model =
    { model | pos = multiply model.friction model.vel }


applyVelocity : Particle -> Particle
applyVelocity model =
    { model | pos = integrate model.pos model.vel }


update : Computer -> Memory -> Memory
update c m =
    let
        mousePoint =
            fromRec c.mouse

        springPoint =
            mousePoint

        k =
            0.1
    in
    m |> springTo springPoint k |> applyFriction |> applyVelocity


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
