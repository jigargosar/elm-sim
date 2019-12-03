module GravitronV2.Main exposing (main)

import Basics.Extra exposing (flip)
import GravitronV2.Draw exposing (..)
import GravitronV2.Health as Health exposing (Health)
import GravitronV2.Timer as Timer exposing (Timer)
import GravitronV2.Vector2 as V exposing (..)



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
    , health : Health.Health
    }


initPlayer : Player
initPlayer =
    { position = vec0
    , velocity = vec 0 -10
    , radius = 10
    , color = red
    , springConstant = 0.2
    , friction = 0.5
    , mass = 300
    , health = Health.init 100
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
            player.radius * Health.normalize player.health
    in
    [ circle x y player.radius (withAlpha 0.5 player.color)
    , circle x y remainingHealthRadius player.color
    ]



-- Turret


type alias Turret =
    { position : Vec
    , radius : Float
    , color : Color
    , health : Health
    , triggerTimer : Timer
    }


initTurret : Float -> Vec -> Turret
initTurret clock position =
    let
        triggerMaxTicks =
            60 * 5
    in
    { position = position
    , radius = 10
    , color = green
    , health = Health.init 1
    , triggerTimer = Timer.start clock triggerMaxTicks
    }


turretRestartTriggerTimer : Float -> Turret -> Turret
turretRestartTriggerTimer clock turret =
    { turret | triggerTimer = Timer.restart clock turret.triggerTimer }


renderTurret : Float -> Turret -> List Shape
renderTurret rTicks turret =
    let
        progress =
            Timer.value rTicks turret.triggerTimer

        ( x, y ) =
            toTuple turret.position

        remainingHealthRadius =
            turret.radius * Health.normalize turret.health
    in
    [ circle x y turret.radius (withAlpha 0.5 turret.color)
    , circle x y remainingHealthRadius turret.color
    , if progress > 0 then
        strokeArc ( x, y ) (turns progress) ( x + turret.radius + (turret.radius / 4), y ) white

      else
        noShape
    , noShape
    ]



-- Bullet


type alias Bullet =
    { position : Vec
    , velocity : Vec
    , maxSpeed : Float
    , radius : Float
    , color : Color
    , health : Health.Health
    , bounceFriction : Float
    , friction : Float
    }


defaultBullet : Bullet
defaultBullet =
    initBullet vec0


initBullet : Vec -> Bullet
initBullet position =
    let
        speed =
            2.8

        maxSpeed =
            7
    in
    { position = position
    , velocity = vec speed speed
    , maxSpeed = maxSpeed
    , radius = 5
    , color = white
    , health = Health.init 1
    , bounceFriction = 0.85
    , friction = 1
    }


updateBullet : Computer -> Player -> Bullet -> Bullet
updateBullet c player bullet =
    let
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

        limitSpeed : Float -> HasVelocity a -> HasVelocity a
        limitSpeed maxSpeed model =
            { model | velocity = V.clampMagnitude maxSpeed model.velocity }
    in
    bullet
        |> bounceWithinScreen c.screen
        |> applyForce (V.gravityFrom bullet.position player.position player.mass)
        |> applyScalarForce bullet.friction
        |> limitSpeed bullet.maxSpeed
        |> applyVelocity


renderBullet : Bullet -> Shape
renderBullet bullet =
    let
        ( x, y ) =
            V.toTuple bullet.position
    in
    circle x y bullet.radius bullet.color



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


stepBulletExplosionAnimation : BulletExplosion -> BulletExplosion
stepBulletExplosionAnimation model =
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
            bullet.radius + (bullet.radius * progress)

        color =
            withAlpha (1 - progress) bullet.color
    in
    circle x y radius color



-- TurretExplosion


type alias TurretExplosion =
    { turret : Turret
    , maxTicks : Float
    , elapsed : Float
    }


explosionFromTurret : Turret -> TurretExplosion
explosionFromTurret turret =
    { turret = turret
    , maxTicks = 60
    , elapsed = 0
    }


stepTurretExplosionAnimation : TurretExplosion -> TurretExplosion
stepTurretExplosionAnimation model =
    { model | elapsed = model.elapsed + 1 }


isTurretExplosionAnimating : TurretExplosion -> Bool
isTurretExplosionAnimating model =
    model.elapsed < model.maxTicks


renderTurretExplosions : TurretExplosion -> Shape
renderTurretExplosions model =
    let
        ( x, y ) =
            V.toTuple turret.position

        turret =
            model.turret

        progress =
            clamp 0 model.maxTicks model.elapsed
                / model.maxTicks

        radius =
            turret.radius + (turret.radius * progress)

        color =
            withAlpha (1 - progress) turret.color
    in
    circle x y radius color



-- Game


type GameState
    = Running
    | GameOver Int



-- Memory


type alias Memory =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , elapsed : Int
    , bulletExplosions : List BulletExplosion
    , turretExplosions : List TurretExplosion
    , state : GameState
    , stage : Int
    , rTicks : Float
    }


allTurretsPositions =
    [ vec -1 -1, vec 1 -1, vec 1 1, vec -1 1 ]
        |> List.map (V.multiply 150)


initTurretsForStage : Int -> Float -> Turrets
initTurretsForStage stage clock =
    allTurretsPositions |> List.take stage |> List.map (initTurret clock)


initMemory : Int -> Memory
initMemory elapsed =
    let
        stage =
            4

        rTicks =
            0
    in
    { player = initPlayer
    , turrets = initTurretsForStage stage rTicks |> List.take stage
    , bullets = []
    , elapsed = elapsed
    , bulletExplosions = []
    , turretExplosions = []
    , stage = stage
    , state = Running
    , rTicks = rTicks
    }



--fireBulletAfterTicks =
--    60 * 1
--
--
--fireBulletModByElapsed elapsedTicks =
--    modBy fireBulletAfterTicks elapsedTicks
--
--
--fireBulletProgress elapsedTicks =
--    1 / fireBulletAfterTicks * toFloat (fireBulletModByElapsed elapsedTicks)
--


fireBulletFromTurretTo : Player -> Turret -> Bullet
fireBulletFromTurretTo player turret =
    let
        bullet =
            defaultBullet

        angle =
            V.vecFrom turret.position player.position
                |> V.angle

        position =
            V.fromRTheta (turret.radius + bullet.radius + 1) angle
                |> V.integrate turret.position

        velocity =
            V.fromRTheta (V.len bullet.velocity) angle
    in
    { bullet
        | position = position
        , velocity = velocity
    }



{-
   fireBullet : Int -> Player -> Turret -> List Bullet -> List Bullet
   fireBullet elapsedTicks player turret bullets =
       if fireBulletModByElapsed elapsedTicks == 0 then
           let
               bullet =
                   defaultBullet

               angle =
                   V.vecFrom turret.position player.position
                       |> V.angle

               position =
                   V.fromRTheta (turret.radius + bullet.radius + 1) angle
                       |> V.integrate turret.position

               velocity =
                   V.fromRTheta (V.len bullet.velocity) angle
           in
           { bullet
               | position = position
               , velocity = velocity
           }
               :: bullets

       else
           bullets


   fireBullets : Int -> Player -> List Bullet -> Turrets -> List Bullet
   fireBullets elapsedTicks player bullets =
       List.foldl (fireBullet elapsedTicks player) bullets
-}


update : Computer -> Memory -> Memory
update c model =
    (case model.state of
        Running ->
            model
                |> stepAnimations
                |> stepTimers
                |> handleUpdate c
                |> handleCollision
                |> handleDeath

        GameOver at ->
            let
                maxGameOverTicks =
                    60 * 3
            in
            if model.elapsed - at > maxGameOverTicks then
                initMemory model.elapsed

            else
                stepAnimations model
    )
        |> incElapsed
        |> incRunningTicks


stepTimers : Memory -> Memory
stepTimers model =
    let
        rTicks =
            model.rTicks

        reducer turret ( bullets, turrets ) =
            if Timer.isDone rTicks turret.triggerTimer then
                ( fireBulletFromTurretTo model.player turret :: bullets
                , turretRestartTriggerTimer rTicks turret :: turrets
                )

            else
                ( bullets, turrets )

        ( firedBullets, newTurrets ) =
            List.foldl reducer ( [], [] ) model.turrets
    in
    { model
        | turrets = newTurrets
        , bullets = firedBullets ++ model.bullets
    }


stepAnimations : Memory -> Memory
stepAnimations model =
    { model
        | bulletExplosions = List.map stepBulletExplosionAnimation model.bulletExplosions
        , turretExplosions = List.map stepTurretExplosionAnimation model.turretExplosions
    }


handleUpdate : Computer -> Memory -> Memory
handleUpdate c model =
    { model
        | player = updatePlayer c model.player
        , bullets = List.map (updateBullet c model.player) model.bullets
    }


incElapsed : Memory -> Memory
incElapsed model =
    { model | elapsed = model.elapsed + 1 }


incRunningTicks : Memory -> Memory
incRunningTicks model =
    if model.state == Running then
        { model | rTicks = model.rTicks + 1 }

    else
        model


handleDeath : Memory -> Memory
handleDeath model =
    let
        ( bullets, deadBullets ) =
            List.partition (.health >> Health.isAlive) model.bullets

        bulletExplosions =
            List.map explosionFromBullet deadBullets
                ++ model.bulletExplosions
                |> List.filter isBulletExplosionAnimating

        ( turrets, deadTurrets ) =
            List.partition (.health >> Health.isAlive) model.turrets

        turretExplosions =
            List.map explosionFromTurret deadTurrets
                ++ model.turretExplosions
                |> List.filter isTurretExplosionAnimating
    in
    { model
        | bullets = bullets
        , bulletExplosions = bulletExplosions
        , turretExplosions = turretExplosions
        , stage =
            if List.isEmpty turrets then
                model.stage + 1

            else
                model.stage
        , turrets =
            if List.isEmpty turrets then
                let
                    nextStage =
                        model.stage + 1 |> modBy 4
                in
                initTurretsForStage nextStage model.rTicks

            else
                turrets
        , state =
            if model.player.health |> Health.isDead then
                GameOver model.elapsed

            else
                model.state
    }


handleBulletsCollision : List Bullet -> List Bullet -> List Bullet
handleBulletsCollision processed remaining =
    case remaining of
        [] ->
            processed

        bullet :: [] ->
            bullet :: processed

        first :: rest ->
            if Health.isAlive first.health then
                let
                    reducer b2 ( b1, acc ) =
                        if circleCircleCollision b1 b2 then
                            ( { b1 | health = Health.kill b1.health }, { b2 | health = Health.kill b1.health } :: acc )

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
                ( { player | health = Health.dec player.health }
                , { bullet | health = Health.kill player.health } :: bulletList
                )

            else
                ( player, bullet :: bulletList )
    in
    \player -> List.foldl reducer ( player, [] )


handleTurretBulletsCollision : Turret -> List Bullet -> ( Turret, List Bullet )
handleTurretBulletsCollision =
    let
        reducer bullet ( turret, bulletList ) =
            if circleCircleCollision bullet turret then
                ( mapHealth Health.dec turret
                , mapHealth Health.dec bullet :: bulletList
                )

            else
                ( turret, bullet :: bulletList )
    in
    \turret -> List.foldl reducer ( turret, [] )


handleTurretsBulletsCollision : Turrets -> List Bullet -> ( Turrets, List Bullet )
handleTurretsBulletsCollision =
    let
        reducer turret ( turretList, bulletList ) =
            handleTurretBulletsCollision turret bulletList
                |> Tuple.mapFirst (flip (::) turretList)
    in
    \turrets bullets -> List.foldl reducer ( [], bullets ) turrets


mapBullets : (a -> a) -> { b | bullets : a } -> { b | bullets : a }
mapBullets func model =
    { model | bullets = func model.bullets }


mapHealth : (a -> a) -> { b | health : a } -> { b | health : a }
mapHealth func model =
    { model | health = func model.health }


mapPlayerAndBullets func model =
    let
        ( player, bullets ) =
            func model.player model.bullets
    in
    { model | player = player, bullets = bullets }


type alias Turrets =
    List Turret


mapPlayerAndTurrets : (Player -> Turrets -> ( Player, Turrets )) -> Memory -> Memory
mapPlayerAndTurrets func model =
    let
        ( player, turrets ) =
            func model.player model.turrets
    in
    { model | player = player, turrets = turrets }


type alias Bullets =
    List Bullet


mapTurretsAndBullets : (Turrets -> Bullets -> ( Turrets, Bullets )) -> Memory -> Memory
mapTurretsAndBullets func model =
    let
        ( turrets, bullets ) =
            func model.turrets model.bullets
    in
    { model | turrets = turrets, bullets = bullets }


handlePlayerTurretsCollision : Player -> Turrets -> ( Player, Turrets )
handlePlayerTurretsCollision =
    let
        reducer turret player =
            if circleCircleCollision player turret then
                mapHealth Health.kill player

            else
                player
    in
    \player turrets -> List.foldl reducer player turrets |> flip Tuple.pair turrets


handleCollision : Memory -> Memory
handleCollision model =
    mapBullets (handleBulletsCollision []) model
        |> mapPlayerAndBullets handlePlayerBulletsCollision
        |> mapTurretsAndBullets handleTurretsBulletsCollision
        |> mapPlayerAndTurrets handlePlayerTurretsCollision


view : Computer -> Memory -> List Shape
view _ model =
    let
        rTicks =
            model.rTicks
    in
    renderPlayer model.player
        ++ List.map renderTurretExplosions model.turretExplosions
        ++ List.concatMap (renderTurret rTicks) model.turrets
        ++ List.map renderBullet model.bullets
        ++ List.map renderBulletExplosions model.bulletExplosions
        ++ viewGameState model.state


viewGameState : GameState -> List Shape
viewGameState state =
    case state of
        Running ->
            [ text 0 0 "Running" ]

        GameOver _ ->
            [ text 0 0 "Game Over" ]


main : Game Memory
main =
    game (initMemory 0) update view
