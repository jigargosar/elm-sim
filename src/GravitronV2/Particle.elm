module GravitronV2.Particle exposing (..)

import GravitronV2.Vector2 exposing (..)


initParticle : Vec -> Vec -> Particle
initParticle =
    Particle


type alias Particle =
    { position : Vec
    , velocity : Vec
    }


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
