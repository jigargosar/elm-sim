module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 exposing (..)



-- Particle


type alias Particle =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : Color
    , springConstant : Float
    , friction : Float
    }


initParticle : Particle
initParticle =
    { position = vec0
    , velocity = vec0
    , radius = 10
    , color = red
    , springConstant = 0.1
    , friction = 0.9
    }


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


renderParticle : Particle -> Shape
renderParticle particle =
    let
        xy =
            toRec particle.position
    in
    circle xy.x xy.y 10 red



-- Game


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
    [ renderParticle m ]


main : Game Memory
main =
    game initialMemory update view
