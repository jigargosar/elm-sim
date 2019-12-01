module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 exposing (..)


type alias Particle =
    { position : Vec
    , velocity : Vec
    , springConstant : Float
    , friction : Float
    }


initParticle =
    Particle (vec 0 0) (vec 0 0) 0.1 0.9


updateParticle : Vec -> Particle -> Particle
updateParticle springPoint particle =
    let
        applyFriction : Float -> Particle -> Particle
        applyFriction friction model =
            { model | position = multiply friction model.velocity }

        applySpringForceTowardsPoint : Vec -> Float -> Particle -> Particle
        applySpringForceTowardsPoint toPoint k model =
            let
                force =
                    springForceFrom model.position toPoint k
            in
            applyForce force model

        applyForce : Vec -> Particle -> Particle
        applyForce force model =
            { model | velocity = integrate force model.velocity }

        applyVelocity : Particle -> Particle
        applyVelocity model =
            { model | position = integrate model.position model.velocity }
    in
    particle
        |> applySpringForceTowardsPoint springPoint particle.springConstant
        |> applyFriction particle.friction
        |> applyVelocity


type alias Memory =
    Particle


initialMemory : Memory
initialMemory =
    initParticle


update : Computer -> Memory -> Memory
update c model =
    let
        springPoint =
            fromRec c.mouse

        particle =
            model
    in
    updateParticle springPoint particle


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
