module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 exposing (..)



-- Player


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


updatePlayer : Computer -> Player -> Player
updatePlayer c particle =
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

        springPoint =
            fromRec c.mouse
    in
    particle
        |> applySpringForceTowardsPoint springPoint particle.springConstant
        |> applyFriction particle.friction
        |> applyVelocity


renderPlayer : Player -> Shape
renderPlayer player =
    let
        xy =
            toRec player.position
    in
    circle xy.x xy.y player.radius player.color



-- Turret


type alias Turret =
    { position : Vec
    , radius : Float
    , color : Color
    }


initTurret : Turret
initTurret =
    { position = vec -100 0
    , radius = 10
    , color = green
    }


renderTurret : Turret -> Shape
renderTurret turret =
    let
        xy =
            toRec turret.position
    in
    circle xy.x xy.y turret.radius turret.color



-- Game


type alias Memory =
    { player : Player
    , turret : Turret
    }


initialMemory : Memory
initialMemory =
    { player = initPlayer
    , turret = initTurret
    }


update : Computer -> Memory -> Memory
update c model =
    { model
        | player = updatePlayer c model.player
    }


view : Computer -> Memory -> List Shape
view c model =
    [ renderPlayer model.player
    ]


main : Game Memory
main =
    game initialMemory update view
