module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 as V exposing (..)



-- Common


type alias HasPositionVelocity a =
    { a
        | position : Vec
        , velocity : Vec
    }


type alias HasFriction a =
    { a | friction : Float }


type alias HasPositionVelocityFriction a =
    HasPositionVelocity (HasFriction a)


applyFriction : HasPositionVelocityFriction a -> HasPositionVelocityFriction a
applyFriction model =
    { model | velocity = multiply model.friction model.velocity }


scaleVelocityBy : Float -> { a | velocity : Vec } -> { a | velocity : Vec }
scaleVelocityBy scale model =
    { model | velocity = multiply scale model.velocity }



-- Player


type alias Player =
    HasPositionVelocity
        { radius : Float
        , color : Color
        , springConstant : Float
        , friction : Float
        , mass : Float
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

        applyForce : Vec -> Player -> Player
        applyForce force model =
            { model | velocity = integrate force model.velocity }

        applyVelocity : HasPositionVelocity a -> HasPositionVelocity a
        applyVelocity model =
            { model | position = integrate model.position model.velocity }

        springPoint =
            fromRec c.mouse
    in
    player
        |> applySpringForceTowardsPoint springPoint player.springConstant
        |> applyFriction
        |> applyVelocity


renderPlayer : Player -> Shape
renderPlayer player =
    let
        ( x, y ) =
            toTuple player.position
    in
    circle x y player.radius player.color



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
    , velocity = vec 10 10
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

        applyForce : Vec -> HasPositionVelocity a -> HasPositionVelocity a
        applyForce force model =
            { model | velocity = integrate force model.velocity }

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
                    |> scaleVelocityBy model.bounceFriction

            else
                model

        applyVelocity : HasPositionVelocity a -> HasPositionVelocity a
        applyVelocity model =
            { model | position = integrate model.position model.velocity }
    in
    bullet
        |> bounceWithinScreen c.screen
        |> applyGravityForce player.position player.mass
        |> applyVelocity
        |> applyFriction


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
    , maxTicks : Int
    , elapsed : Int
    }


explosionFromBullet : Bullet -> BulletExplosion
explosionFromBullet bullet =
    { bullet = bullet
    , maxTicks = 60
    , elapsed = 0
    }



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
            10

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
            List.map explosionFromBullet deadBullets ++ model.bulletExplosions
    in
    { model
        | bullets = bullets
        , bulletExplosions = bulletExplosions
    }


circleCircleCollision c1 c2 =
    V.lenFrom c1.position c2.position <= c1.radius + c2.radius


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


handlePlayerBulletsCollision player bullets =
    let
        reducer b ( p, bArr ) =
            if circleCircleCollision b p then
                ( p, b :: bArr )

            else
                ( p, b :: bArr )
    in
    List.foldl reducer ( player, [] ) bullets


handleCollision : Memory -> Memory
handleCollision model =
    let
        ( player, bullets ) =
            handleBulletsCollision [] model.bullets
                |> handlePlayerBulletsCollision model.player
    in
    { model
        | bullets = bullets
        , player = player
    }


view : Computer -> Memory -> List Shape
view _ model =
    renderPlayer model.player
        :: renderTurret model.turret
        :: List.map renderBullet model.bullets


main : Game Memory
main =
    game initialMemory update view
