module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 as V exposing (..)
import PointFree exposing (clamp0, dec)



-- HealthSystem


type Health
    = Health Float Float


initHealth : Float -> Health
initHealth maxHealth =
    let
        absMaxHealth =
            abs maxHealth
    in
    Health absMaxHealth absMaxHealth


normalizedHealth : Health -> Float
normalizedHealth (Health maxHealth health) =
    clamp0 maxHealth health / maxHealth


mapCurrentHealth : (Float -> Float) -> Health -> Health
mapCurrentHealth func (Health maxHealth health) =
    func health
        |> clamp0 maxHealth
        |> Health maxHealth


decHealth : Health -> Health
decHealth =
    mapCurrentHealth dec



-- Universal Interfaces


type alias HasPosition a =
    { a | position : Vec }



-- Geometric Interfaces


type alias HasRadius a =
    { a | radius : Float }


type alias Circular a =
    HasPosition (HasRadius a)


circleCircleCollision : Circular a -> Circular b -> Bool
circleCircleCollision c1 c2 =
    V.lenFrom c1.position c2.position <= c1.radius + c2.radius



-- Physics Velocity : Apply Force


type alias HasVelocity a =
    { a | velocity : Vec }


applyForce : Vec -> HasVelocity a -> HasVelocity a
applyForce force model =
    { model | velocity = integrate force model.velocity }


applyScalarForce : Float -> HasVelocity a -> HasVelocity a
applyScalarForce force model =
    { model | velocity = V.multiply force model.velocity }



-- Physics Velocity: Update Position


type alias HasPositionVelocity a =
    HasPosition (HasVelocity a)


applyVelocity : HasPositionVelocity a -> HasPositionVelocity a
applyVelocity model =
    { model | position = integrate model.position model.velocity }



-- Player


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : Color
    , friction : Float
    , springConstant : Float
    , mass : Float
    , health : Health
    }


initPlayer : Player
initPlayer =
    { position = vec0
    , velocity = vec0
    , radius = 10
    , color = red
    , springConstant = 0.1
    , friction = 0.5
    , mass = 2000
    , health = initHealth 100
    }


updatePlayer : Computer -> Player -> Player
updatePlayer c player =
    let
        applySpringForceTowardsPoint : Vec -> Float -> Player -> Player
        applySpringForceTowardsPoint toPoint k model =
            let
                force =
                    springForceFrom model.position toPoint k
            in
            applyForce force model

        springPoint =
            fromRec c.mouse
    in
    player
        |> applySpringForceTowardsPoint springPoint player.springConstant
        |> applyScalarForce player.friction
        |> applyVelocity


renderPlayer : Player -> List Shape
renderPlayer player =
    let
        ( x, y ) =
            toTuple player.position

        remainingHealthRadius =
            player.radius
                * normalizedHealth player.health
    in
    [ circle x y player.radius (withAlpha 0.5 player.color)
    , circle x y remainingHealthRadius player.color
    ]



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
        ( x, y ) =
            toTuple turret.position
    in
    circle x y turret.radius turret.color



-- Bullet


type alias Bullet =
    HasPositionVelocity
        { radius : Float
        , color : Color
        , isAlive : Bool
        , bounceFriction : Float
        , friction : Float
        }


initBullet : Vec -> Bullet
initBullet position =
    { position = position
    , velocity = vec 5 5
    , radius = 5
    , color = white
    , isAlive = True
    , bounceFriction = 0.8
    , friction = 1
    }


updateBullet : Computer -> Player -> Bullet -> Bullet
updateBullet c player bullet =
    let
        applyGravityForce : Vec -> Float -> HasPositionVelocity a -> HasPositionVelocity a
        applyGravityForce toPoint mass model =
            let
                gv =
                    V.vecFrom model.position toPoint

                force =
                    V.fromRTheta (mass / V.len2 gv) (V.angle gv)
            in
            applyForce force model

        bounceWithinScreen : Screen -> Bullet -> Bullet
        bounceWithinScreen screen model =
            let
                bounceVelocityPart lo high positionPart velocityPart =
                    if
                        (positionPart < lo && velocityPart < 0)
                            || (positionPart > high && velocityPart > 0)
                    then
                        negate velocityPart

                    else
                        velocityPart

                ( x, y ) =
                    V.toTuple model.position

                ( vx, vy ) =
                    V.toTuple model.velocity

                velocity =
                    vec (bounceVelocityPart screen.left screen.right x vx)
                        (bounceVelocityPart screen.top screen.bottom y vy)
            in
            if velocity /= model.velocity then
                { model | velocity = velocity }
                    |> applyScalarForce model.bounceFriction

            else
                model
    in
    bullet
        |> bounceWithinScreen c.screen
        |> applyGravityForce player.position player.mass
        |> applyVelocity
        |> applyScalarForce bullet.friction


renderBullet : Bullet -> Shape
renderBullet bullet =
    let
        xy =
            toRec bullet.position
    in
    circle xy.x xy.y bullet.radius bullet.color



-- BulletExplosion


type alias BulletExplosion =
    { bullet : Bullet
    , maxTicks : Float
    , elapsed : Float
    }


explosionFromBullet : Bullet -> BulletExplosion
explosionFromBullet bullet =
    { bullet = bullet
    , maxTicks = 60
    , elapsed = 0
    }


updateBulletExplosion : BulletExplosion -> BulletExplosion
updateBulletExplosion model =
    { model | elapsed = model.elapsed + 1 }


isBulletExplosionAnimating : BulletExplosion -> Bool
isBulletExplosionAnimating model =
    model.elapsed < model.maxTicks


renderBulletExplosions : BulletExplosion -> Shape
renderBulletExplosions model =
    let
        ( x, y ) =
            V.toTuple bullet.position

        bullet =
            model.bullet

        progress =
            clamp 0 model.maxTicks model.elapsed
                / model.maxTicks

        radius =
            bullet.radius + (2 * progress)

        color =
            withAlpha (1 - progress) bullet.color
    in
    circle x y radius color



-- Game


type alias Memory =
    { player : Player
    , turret : Turret
    , bullets : List Bullet
    , elapsed : Int
    , bulletExplosions : List BulletExplosion
    }


initialMemory : Memory
initialMemory =
    { player = initPlayer
    , turret = initTurret
    , bullets = []
    , elapsed = 0
    , bulletExplosions = []
    }


fireBullet : Int -> Vec -> List Bullet -> List Bullet
fireBullet elapsedTicks position bullets =
    let
        fireBulletRate =
            27

        bulletCount =
            List.length bullets

        maxBullets =
            100

        shouldAddBullet =
            modBy fireBulletRate elapsedTicks == 0 && bulletCount < maxBullets
    in
    if shouldAddBullet then
        initBullet position :: bullets

    else
        bullets


update : Computer -> Memory -> Memory
update c model =
    { model
        | player = updatePlayer c model.player
        , bullets =
            List.map (updateBullet c model.player) model.bullets
                |> fireBullet model.elapsed model.turret.position
        , bulletExplosions = List.map updateBulletExplosion model.bulletExplosions
        , elapsed = model.elapsed + 1
    }
        |> handleCollision
        |> handleDeath


handleDeath : Memory -> Memory
handleDeath model =
    let
        ( bullets, deadBullets ) =
            List.partition .isAlive model.bullets

        bulletExplosions =
            List.map explosionFromBullet deadBullets
                ++ model.bulletExplosions
                |> List.filter isBulletExplosionAnimating
    in
    { model
        | bullets = bullets
        , bulletExplosions = bulletExplosions
    }


handleBulletsCollision : List Bullet -> List Bullet -> List Bullet
handleBulletsCollision processed remaining =
    case remaining of
        [] ->
            processed

        bullet :: [] ->
            bullet :: processed

        first :: rest ->
            if first.isAlive then
                let
                    reducer b2 ( b1, acc ) =
                        if circleCircleCollision b1 b2 then
                            ( { b1 | isAlive = False }, { b2 | isAlive = False } :: acc )

                        else
                            ( b1, b2 :: acc )

                    ( processedBullet, newRemaining ) =
                        List.foldl reducer ( first, [] ) rest
                in
                handleBulletsCollision (processedBullet :: processed) newRemaining

            else
                handleBulletsCollision (first :: processed) rest


handlePlayerBulletsCollision : Player -> List Bullet -> ( Player, List Bullet )
handlePlayerBulletsCollision =
    let
        reducer bullet ( player, bulletList ) =
            if circleCircleCollision bullet player then
                ( { player | health = decHealth player.health }
                , { bullet | isAlive = False } :: bulletList
                )

            else
                ( player, bullet :: bulletList )
    in
    \player -> List.foldl reducer ( player, [] )


handleCollision : Memory -> Memory
handleCollision model =
    let
        ( player, bullets ) =
            handleBulletsCollision [] model.bullets
                |> handlePlayerBulletsCollision model.player
    in
    { model
        | player = player
        , bullets = bullets
    }


view : Computer -> Memory -> List Shape
view _ model =
    renderPlayer model.player
        ++ [ renderTurret model.turret ]
        ++ List.map renderBullet model.bullets
        ++ List.map renderBulletExplosions model.bulletExplosions


main : Game Memory
main =
    game initialMemory update view
