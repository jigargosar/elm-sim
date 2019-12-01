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
updatePlayer c player =
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
    player
        |> applySpringForceTowardsPoint springPoint player.springConstant
        |> applyFriction player.friction
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
    { position = vec -200 0
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



-- Bullet


type alias Bullet =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : Color
    }


initBullet : Bullet
initBullet =
    { position = vec -200 0
    , velocity = vec1
    , radius = 5
    , color = white
    }


updateBullet : Bullet -> Bullet
updateBullet bullet =
    let
        applyVelocity : Bullet -> Bullet
        applyVelocity model =
            { model | position = integrate model.position model.velocity }
    in
    bullet |> applyVelocity


renderBullet : Bullet -> Shape
renderBullet bullet =
    let
        xy =
            toRec bullet.position
    in
    circle xy.x xy.y bullet.radius bullet.color



-- Game


type alias Memory =
    { player : Player
    , turret : Turret
    , bullet : Bullet
    }


initialMemory : Memory
initialMemory =
    { player = initPlayer
    , turret = initTurret
    , bullet = initBullet
    }


update : Computer -> Memory -> Memory
update c model =
    { model
        | player = updatePlayer c model.player
        , bullet = updateBullet model.bullet
    }


view : Computer -> Memory -> List Shape
view _ model =
    [ renderPlayer model.player
    , renderTurret model.turret
    , renderBullet model.bullet
    ]


main : Game Memory
main =
    game initialMemory update view
