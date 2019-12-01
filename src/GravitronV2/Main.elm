module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 exposing (..)



-- Particle


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : Color
    , springConstant : Float
    , friction : Float
    }


initPlayer : Player
initPlayer =
    { position = vec0
    , velocity = vec0
    , radius = 10
    , color = red
    , springConstant = 0.1
    , friction = 0.9
    }


updatePlayer : Vec -> Player -> Player
updatePlayer springPoint particle =
    let
        applyFriction : Float -> Player -> Player
        applyFriction friction model =
            { model | position = multiply friction model.velocity }

        applySpringForceTowardsPoint : Vec -> Float -> Player -> Player
        applySpringForceTowardsPoint toPoint k model =
            let
                force =
                    springForceFrom model.position toPoint k
            in
            applyForce force model

        applyForce : Vec -> Player -> Player
        applyForce force model =
            { model | velocity = integrate force model.velocity }

        applyVelocity : Player -> Player
        applyVelocity model =
            { model | position = integrate model.position model.velocity }
    in
    particle
        |> applySpringForceTowardsPoint springPoint particle.springConstant
        |> applyFriction particle.friction
        |> applyVelocity


renderPlayer : Player -> Shape
renderPlayer particle =
    let
        xy =
            toRec particle.position
    in
    circle xy.x xy.y 10 red



-- Game


type alias Memory =
    Player


initialMemory : Memory
initialMemory =
    initPlayer


update : Computer -> Memory -> Memory
update c model =
    let
        springPoint =
            fromRec c.mouse

        particle =
            model
    in
    updatePlayer springPoint particle


view : Computer -> Memory -> List Shape
view c m =
    [ renderPlayer m ]


main : Game Memory
main =
    game initialMemory update view
