module GravitronV2.Main exposing (main)

import GravitronV2.Game as G exposing (Screen)
import GravitronV2.HasHealth as HasHealth
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



-- Player


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : G.Color
    , health : HasHealth.Health
    }


initPlayer : Player
initPlayer =
    { position = vec0
    , velocity = vec 0 -10
    , radius = 10
    , color = G.red
    , health = HasHealth.init 100
    }


updatePlayer : G.Mouse -> Player -> Player
updatePlayer mouse player =
    let
        springToMouseForce =
            V.vecFrom player.position (V.fromRec mouse)
                -- springConstant
                |> V.multiply 0.2

        newVelocity =
            [ V.add springToMouseForce
            , V.multiply 0.5 -- friction
            ]
                |> List.foldl (<|) player.velocity
    in
    { player
        | velocity = newVelocity
        , position = V.add player.position newVelocity
    }


renderPlayer : Player -> List G.Shape
renderPlayer player =
    let
        ( x, y ) =
            V.toTuple player.position

        remainingHealthRadius =
            player.radius * HasHealth.normalized player
    in
    [ G.circle x y player.radius (G.withAlpha 0.5 player.color)
    , G.circle x y remainingHealthRadius player.color
    ]



-- Turret


type alias Turret =
    { position : Vec
    , radius : Float
    , color : G.Color
    , health : HasHealth.Health
    , triggerTimer : Timer
    }


initTurret : Timer -> Vec -> Turret
initTurret triggerTimer position =
    { position = position
    , radius = 10
    , color = G.green
    , health = HasHealth.init 1
    , triggerTimer = triggerTimer
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
            turret.radius * HasHealth.normalized turret
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
    , health : HasHealth.Health
    , bounceFriction : Float
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
    , health = HasHealth.init 1
    , bounceFriction = 0.85
    }


bounceWithinScreen : G.Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreen screen position bounceFriction velocity =
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
        newBouncedVelocity |> V.multiply bounceFriction

    else
        newBouncedVelocity


updateBullet : G.Screen -> Player -> Bullet -> Bullet
updateBullet screen player bullet =
    let
        gravityVec =
            V.vecFrom bullet.position player.position
                |> V.mapMagnitude (\m -> 20 / m)

        newVelocity =
            [ bounceWithinScreen screen bullet.position bullet.bounceFriction
            , V.add gravityVec
            ]
                |> List.foldl (<|) bullet.velocity
    in
    { bullet
        | velocity = newVelocity
        , position = V.add bullet.position newVelocity
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
initTurretsForStage stage rTicks =
    let
        maxTurrets =
            List.length allTurretsPositions

        turretCountForStage =
            modBy maxTurrets (stage - 1) + 1

        triggerTimerDuration =
            60 * 5

        delayPerTurret =
            triggerTimerDuration / toFloat turretCountForStage

        triggerTimer : Int -> Timer
        triggerTimer idx =
            Timer.delayedStart rTicks
                triggerTimerDuration
                (toFloat idx * delayPerTurret)

        initTurretAtIdx : Int -> Vec -> Turret
        initTurretAtIdx =
            triggerTimer >> initTurret
    in
    allTurretsPositions
        |> List.take turretCountForStage
        |> List.indexedMap initTurretAtIdx


initMemory : Memory
initMemory =
    let
        stage =
            4

        rTicks =
            0
    in
    { player = initPlayer
    , turrets = initTurretsForStage stage rTicks |> Debug.log "it"
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
    model
        |> mapPlayerAndBullets (foldMap (onCircularCollisionMapBoth HasHealth.dec HasHealth.kill))
        |> mapPlayerAndTurrets (foldMap (onCircularCollisionMapBoth HasHealth.kill identity))
        |> mapTurretsAndBullets (foldMapList (onCircularCollisionMapBoth HasHealth.dec HasHealth.dec))
        |> mapBullets (foldMapSelf (onCircularCollisionMapBoth HasHealth.kill HasHealth.kill))


handleDeath : Memory -> Memory
handleDeath model =
    let
        ( newBullets, newBulletExplosions ) =
            List.partition HasHealth.isAlive model.bullets
                |> Tuple.mapSecond (List.map explosionFromBullet)

        ( newTurrets, newTurretExplosions ) =
            List.partition HasHealth.isAlive model.turrets
                |> Tuple.mapSecond (List.map explosionFromTurret)
    in
    { model
        | bullets = newBullets
        , bulletExplosions =
            newBulletExplosions
                ++ model.bulletExplosions
                |> List.filter isBulletExplosionAnimating
        , turretExplosions =
            newTurretExplosions
                ++ model.turretExplosions
                |> List.filter isTurretExplosionAnimating
        , stage =
            if List.isEmpty newTurrets then
                model.stage + 1

            else
                model.stage
        , turrets =
            if List.isEmpty newTurrets then
                initTurretsForStage (model.stage + 1) model.rTicks

            else
                newTurrets
        , state =
            if model.player |> HasHealth.isDead then
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


foldMap : (a -> b -> ( a, b )) -> a -> List b -> ( a, List b )
foldMap func =
    let
        reducer b ( a, bList ) =
            func a b
                |> Tuple.mapSecond (\newB -> newB :: bList)
    in
    \a bList -> List.foldl reducer ( a, [] ) bList


foldMapList : (a -> b -> ( a, b )) -> List a -> List b -> ( List a, List b )
foldMapList func =
    let
        reducer a ( listA, listB ) =
            foldMap func a listB
                |> Tuple.mapFirst (\newA -> newA :: listA)
    in
    \listA listB -> List.foldl reducer ( [], listB ) listA


foldMapSelf : (a -> a -> ( a, a )) -> List a -> List a
foldMapSelf func list =
    foldMapSelfHelp func ( [], list )


foldMapSelfHelp : (a -> a -> ( a, a )) -> ( List a, List a ) -> List a
foldMapSelfHelp func ( processedList, pendingList ) =
    case pendingList of
        [] ->
            processedList

        first :: rest ->
            foldMap func first rest
                |> Tuple.mapFirst (\newFirst -> newFirst :: processedList)
                |> foldMapSelfHelp func


onCircularCollisionMapBoth :
    (Circular a -> Circular a)
    -> (Circular b -> Circular b)
    -> Circular a
    -> Circular b
    -> ( Circular a, Circular b )
onCircularCollisionMapBoth funcA funcB a b =
    if circleCircleCollision a b then
        ( funcA a, funcB b )

    else
        ( a, b )


mapBullets : (a -> a) -> { b | bullets : a } -> { b | bullets : a }
mapBullets func model =
    { model | bullets = func model.bullets }


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



-- View


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
