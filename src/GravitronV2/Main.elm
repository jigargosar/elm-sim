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
    { position : Vec
    , velocity : Vec
    , friction : Float
    }


springTo : Vec -> Float -> Particle -> Particle
springTo springPoint k model =
    { model
        | velocity =
            springForceFrom model.position springPoint k
                |> integrate model.velocity
    }


applyFriction : Particle -> Particle
applyFriction model =
    { model | position = multiply model.friction model.velocity }


applyVelocity : Particle -> Particle
applyVelocity model =
    { model | position = integrate model.position model.velocity }


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
            toRec m.position
    in
    [ circle xy.x xy.y 10 red ]


main : Game Memory
main =
    game initialMemory update view
