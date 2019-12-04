module GravitronV2.Main exposing (main)

import Basics.Extra exposing (flip)
import GravitronV2.Game as G exposing (Screen)
import GravitronV2.Health as Health exposing (Health)
import GravitronV2.Timer as Timer exposing (Timer)
import GravitronV2.Vector2 as V exposing (Vec, vec, vec0)



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
    { model | velocity = V.add force model.velocity }


applyScalarForce : Float -> HasVelocity a -> HasVelocity a
applyScalarForce force model =
    { model | velocity = V.multiply force model.velocity }



-- Physics Velocity: Update Position


type alias HasPositionVelocity a =
    HasPosition (HasVelocity a)


applyVelocity : HasPositionVelocity a -> HasPositionVelocity a
applyVelocity model =
    { model | position = V.add model.position model.velocity }



-- Player


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : G.Color
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
    , color = G.red
    , springConstant = 0.2
    , friction = 0.5
    , mass = 300
    , health = Health.init 100
    }


updatePlayer : G.Mouse -> Player -> Player
updatePlayer mouse player =
    let
        applySpringForceTowardsPoint : Vec -> Float -> Player -> Player
        applySpringForceTowardsPoint toPoint k model =
            let
                force =
                    V.springForceFrom model.position toPoint k
            in
            applyForce force model

        springPoint =
            V.fromRec mouse
    in
    player
        |> applySpringForceTowardsPoint springPoint player.springConstant
        |> applyScalarForce player.friction
        |> applyVelocity


renderPlayer : Player -> List G.Shape
renderPlayer player =
    let
        ( x, y ) =
            V.toTuple player.position

        remainingHealthRadius =
            player.radius * Health.normalize player.health
    in
    [ G.circle x y player.radius (G.withAlpha 0.5 player.color)
    , G.circle x y remainingHealthRadius player.color
    ]



-- Turret


type alias Turret =
    { position : Vec
    , radius : Float
    , color : G.Color
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
    , color = G.green
    , health = Health.init 1
    , triggerTimer = Timer.start clock triggerMaxTicks
    }


turretRestartTriggerTimer : Float -> Turret -> Turret
turretRestartTriggerTimer clock turret =
    { turret | triggerTimer = Timer.restart clock turret.triggerTimer }


turretTriggerTimerDone : Float -> Turret -> Bool
turretTriggerTimerDone rTicks turret =
    Timer.isDone rTicks turret.triggerTimer


turretRestartTriggerTimerIfDone : Float -> Turret -> Turret
turretRestartTriggerTimerIfDone rTicks turret =
    if Timer.isDone rTicks turret.triggerTimer then
        turretRestartTriggerTimer rTicks turret

    else
        turret


renderTurret : Float -> Turret -> List G.Shape
renderTurret rTicks turret =
    let
        progress =
            Timer.value rTicks turret.triggerTimer

        ( x, y ) =
            V.toTuple turret.position

        remainingHealthRadius =
            turret.radius * Health.normalize turret.health
    in
    [ G.circle x y turret.radius (G.withAlpha 0.5 turret.color)
    , G.circle x y remainingHealthRadius turret.color
    , if progress > 0 then
        G.strokeArc ( x, y ) (turns progress) ( x + turret.radius + (turret.radius / 4), y ) G.white

      else
        G.noShape
    ]



-- Bullet


type alias Bullet =
    { position : Vec
    , velocity : Vec
    , maxSpeed : Float
    , radius : Float
    , color : G.Color
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
    , color = G.white
    , health = Health.init 1
    , bounceFriction = 0.85
    , friction = 1
    }


bounceWithinScreen : G.Screen -> { a | position : Vec, bounceFriction : Float } -> Vec -> Vec
bounceWithinScreen screen { position, bounceFriction } velocity =
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
            V.toTuple position

        ( vx, vy ) =
            V.toTuple velocity

        newBouncedVelocity =
            vec (bounceVelocityPart screen.left screen.right x vx)
                (bounceVelocityPart screen.top screen.bottom y vy)
    in
    if velocity /= newBouncedVelocity then
        newBouncedVelocity
            |> V.multiply bounceFriction

    else
        newBouncedVelocity


updateBullet : G.Screen -> Player -> Bullet -> Bullet
updateBullet screen player bullet =
    let
        newVelocity =
            [ bounceWithinScreen screen bullet
            , V.add (V.gravityFrom bullet.position player.position player.mass)
            , V.multiply bullet.friction
            , V.clampMagnitude bullet.maxSpeed
            ]
                |> List.foldl (\f v -> f v) bullet.velocity

        newPosition =
            V.add bullet.position newVelocity
    in
    { bullet
        | velocity = newVelocity
        , position = newPosition
    }


updateBullets : Screen -> Float -> Player -> Turrets -> Bullets -> Bullets
updateBullets screen rTicks player turrets bullets =
    let
        firedBullets =
            List.foldl
                (prependWhen (turretTriggerTimerDone rTicks)
                    (\t ->
                        fireNewBullet
                            { from = t.position
                            , to = player.position
                            , offset = t.radius
                            }
                    )
                )
                []
                turrets
    in
    firedBullets
        ++ bullets
        |> List.map (updateBullet screen player)


renderBullet : Bullet -> G.Shape
renderBullet bullet =
    let
        ( x, y ) =
            V.toTuple bullet.position
    in
    G.circle x y bullet.radius bullet.color



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


renderBulletExplosions : BulletExplosion -> G.Shape
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
            G.withAlpha (1 - progress) bullet.color
    in
    G.circle x y radius color



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


renderTurretExplosions : TurretExplosion -> G.Shape
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
            G.withAlpha (1 - progress) turret.color
    in
    G.circle x y radius color



-- GameState


type GameState
    = Running
    | GameOver Int
    | Paused



-- Memory


type alias Memory =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
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
initTurretsForStage stage_ rTicks =
    let
        stage =
            stage_ + 1 |> modBy (List.length allTurretsPositions)
    in
    allTurretsPositions |> List.take stage |> List.map (initTurret rTicks)


initMemory : Memory
initMemory =
    let
        stage =
            4

        rTicks =
            0
    in
    { player = initPlayer
    , turrets = initTurretsForStage stage rTicks
    , bullets = []
    , bulletExplosions = []
    , turretExplosions = []
    , stage = stage
    , state = Running
    , rTicks = rTicks
    }


fireNewBullet : { from : Vec, to : Vec, offset : Float } -> Bullet
fireNewBullet { from, to, offset } =
    let
        bullet =
            defaultBullet

        angle =
            V.vecFrom from to
                |> V.angle

        position =
            V.fromRTheta (offset + bullet.radius + 1) angle
                |> V.add from

        velocity =
            V.fromRTheta (V.len bullet.velocity) angle
    in
    { bullet | position = position, velocity = velocity }


spacePressed =
    G.freshKeyDown " "


updateMemory : G.Computer -> Memory -> Memory
updateMemory computer model =
    case model.state of
        Running ->
            if spacePressed computer then
                { model | state = Paused }

            else
                model
                    |> updateEntities computer
                    |> handleCollision
                    |> handleDeath
                    |> incRunningTicks

        GameOver elapsed ->
            let
                maxTicks =
                    60 * 3
            in
            if elapsed > maxTicks then
                initMemory

            else
                { model | state = GameOver (elapsed + 1) }
                    |> stepAnimations

        Paused ->
            if spacePressed computer then
                { model | state = Running }

            else
                model


prependWhen : (c -> Bool) -> (c -> a) -> c -> List a -> List a
prependWhen pred t v =
    if pred v then
        (::) (t v)

    else
        identity


stepAnimations : Memory -> Memory
stepAnimations model =
    { model
        | bulletExplosions = List.map stepBulletExplosionAnimation model.bulletExplosions
        , turretExplosions = List.map stepTurretExplosionAnimation model.turretExplosions
    }


updateEntities : G.Computer -> Memory -> Memory
updateEntities computer model =
    let
        { mouse, screen } =
            computer

        { player, rTicks, turrets, bullets } =
            model
    in
    { model
        | player = updatePlayer mouse player
        , turrets = List.map (turretRestartTriggerTimerIfDone rTicks) turrets
        , bullets = updateBullets screen rTicks player turrets bullets
    }
        |> stepAnimations


handleCollision : Memory -> Memory
handleCollision model =
    mapBullets (Tuple.pair [] >> handleBulletsCollision) model
        |> mapPlayerAndBullets handlePlayerBulletsCollision
        |> mapTurretsAndBullets handleTurretsBulletsCollision
        |> mapPlayerAndTurrets handlePlayerTurretsCollision


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
                initTurretsForStage (model.stage + 1) model.rTicks

            else
                turrets
        , state =
            if model.player.health |> Health.isDead then
                GameOver 0

            else
                model.state
    }


incRunningTicks : Memory -> Memory
incRunningTicks model =
    if model.state == Running then
        { model | rTicks = model.rTicks + 1 }

    else
        model



-- Collision Helpers


type alias HasHealth a =
    { a | health : Health }


killHealth : HasHealth a -> HasHealth a
killHealth =
    mapHealth Health.kill


decHealth : HasHealth a -> HasHealth a
decHealth =
    mapHealth Health.dec


onCircularCollisionMapBoth :
    (Circular a -> Circular a)
    -> (Circular b -> Circular b)
    -> ( Circular a, Circular b )
    -> ( Circular a, Circular b )
onCircularCollisionMapBoth func1 func2 ( c1, c2 ) =
    if circleCircleCollision c1 c2 then
        ( func1 c1, func2 c2 )

    else
        ( c1, c2 )


onCircularAndCircularListCollisionMapBoth :
    (Circular a -> Circular a)
    -> (Circular b -> Circular b)
    -> ( Circular a, List (Circular b) )
    -> ( Circular a, List (Circular b) )
onCircularAndCircularListCollisionMapBoth funcA funcB =
    let
        reducer b ( a, bList ) =
            if circleCircleCollision a b then
                ( funcA a, funcB b :: bList )

            else
                ( a, b :: bList )
    in
    \( a, bList ) -> List.foldl reducer ( a, [] ) bList


handleBulletsCollision : ( List Bullet, List Bullet ) -> List Bullet
handleBulletsCollision ( processed, remaining ) =
    case remaining of
        [] ->
            processed

        first :: rest ->
            onCircularAndCircularListCollisionMapBoth killHealth killHealth ( first, rest )
                |> Tuple.mapFirst (flip (::) processed)
                |> handleBulletsCollision


handlePlayerBulletsCollision : Player -> List Bullet -> ( Player, List Bullet )
handlePlayerBulletsCollision player bullets =
    onCircularAndCircularListCollisionMapBoth decHealth killHealth ( player, bullets )


handleTurretBulletsCollision : Turret -> List Bullet -> ( Turret, List Bullet )
handleTurretBulletsCollision turret bullets =
    onCircularAndCircularListCollisionMapBoth decHealth decHealth ( turret, bullets )


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
handlePlayerTurretsCollision player turrets =
    onCircularAndCircularListCollisionMapBoth killHealth identity ( player, turrets )


viewMemory : G.Computer -> Memory -> List G.Shape
viewMemory _ model =
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


viewGameState : GameState -> List G.Shape
viewGameState state =
    case state of
        Running ->
            [ G.text 0 0 "Running" ]

        GameOver _ ->
            [ G.text 0 0 "Game Over" ]

        Paused ->
            [ G.text 0 0 "Paused" ]



-- MAIN


main =
    G.game initMemory updateMemory viewMemory
