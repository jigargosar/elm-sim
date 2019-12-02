module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Health as Health exposing (Health)
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
    , velocity = vec0
    , radius = 10
    , color = red
    , springConstant = 0.1
    , friction = 0.5
    , mass = 2000
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
    }


initTurret : Turret
initTurret =
    { position = vec -100 -150
    , radius = 10
    , color = green
    , health = Health.init 3
    }


renderTurret : Turret -> List Shape
renderTurret turret =
    let
        ( x, y ) =
            toTuple turret.position

        remainingHealthRadius =
            turret.radius * Health.normalize turret.health
    in
    [ circle x y turret.radius (withAlpha 0.5 turret.color)
    , circle x y remainingHealthRadius turret.color
    ]



-- Bullet


type alias Bullet =
    { position : Vec
    , velocity : Vec
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
    { position = position
    , velocity = vec 5 5
    , radius = 5
    , color = white
    , health = Health.init 1
    , bounceFriction = 0.9
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
    in
    bullet
        |> bounceWithinScreen c.screen
        |> applyForce (V.gravityFrom bullet.position player.position player.mass)
        |> applyScalarForce bullet.friction
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
            bullet.radius + (2 * progress)

        color =
            withAlpha (1 - progress) bullet.color
    in
    circle x y radius color



-- Game


type GameState
    = Running
    | GameOver Int



-- Memory


type alias Memory =
    { player : Player
    , turret : Turret
    , bullets : List Bullet
    , elapsed : Int
    , bulletExplosions : List BulletExplosion
    , state : GameState
    }


initMemory : Int -> Memory
initMemory elapsed =
    { player = initPlayer
    , turret = initTurret
    , bullets = []
    , elapsed = elapsed
    , bulletExplosions = []
    , state = Running
    }


fireBullet : Int -> Turret -> Player -> List Bullet -> List Bullet
fireBullet elapsedTicks turret player bullets =
    let
        oncePerXTicks =
            10

        bulletCount =
            List.length bullets

        maxBullets =
            100

        shouldAddBullet =
            modBy oncePerXTicks elapsedTicks == 0 && bulletCount < maxBullets
    in
    if shouldAddBullet then
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


update : Computer -> Memory -> Memory
update c model =
    (case model.state of
        Running ->
            { model
                | player = updatePlayer c model.player
                , bullets =
                    List.map (updateBullet c model.player) model.bullets
                        |> fireBullet model.elapsed model.turret model.player
                , bulletExplosions = List.map stepBulletExplosionAnimation model.bulletExplosions
            }
                |> handleCollision
                |> handleDeath
                |> handleGameOver

        GameOver at ->
            let
                maxGameOverTicks =
                    60 * 3
            in
            if model.elapsed - at > maxGameOverTicks then
                initMemory model.elapsed

            else
                { model
                    | bulletExplosions = List.map stepBulletExplosionAnimation model.bulletExplosions
                }
    )
        |> incElapsed


incElapsed : Memory -> Memory
incElapsed model =
    { model | elapsed = model.elapsed + 1 }


handleGameOver : Memory -> Memory
handleGameOver model =
    if model.player.health |> Health.isAlive then
        model

    else
        { model | state = GameOver model.elapsed }


handleDeath : Memory -> Memory
handleDeath model =
    let
        ( bullets, deadBullets ) =
            List.partition (.health >> Health.isAlive) model.bullets

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


mapPlayerAndTurret : (Player -> Turret -> ( Player, Turret )) -> Memory -> Memory
mapPlayerAndTurret func model =
    let
        ( player, turret ) =
            func model.player model.turret
    in
    { model | player = player, turret = turret }


type alias Bullets =
    List Bullet


mapTurretAndBullets : (Turret -> Bullets -> ( Turret, Bullets )) -> Memory -> Memory
mapTurretAndBullets func model =
    let
        ( turret, bullets ) =
            func model.turret model.bullets
    in
    { model | turret = turret, bullets = bullets }


handlePlayerTurretCollision : Player -> Turret -> ( Player, Turret )
handlePlayerTurretCollision player turret =
    if circleCircleCollision player turret then
        ( mapHealth Health.kill player, turret )

    else
        ( player, turret )


handleCollision : Memory -> Memory
handleCollision model =
    mapBullets (handleBulletsCollision []) model
        |> mapPlayerAndBullets handlePlayerBulletsCollision
        |> mapTurretAndBullets handleTurretBulletsCollision
        |> mapPlayerAndTurret handlePlayerTurretCollision


view : Computer -> Memory -> List Shape
view _ model =
    renderPlayer model.player
        ++ renderTurret model.turret
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
