module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)
import GravitronV2.Vector2 as V exposing (..)
import PointFree exposing (when)



-- Common


type alias HasPositionVelocity a =
    { a
        | position : Vec
        , velocity : Vec
    }



-- Player


type alias Player =
    HasPositionVelocity
        { radius : Float
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

        applyVelocity : HasPositionVelocity a -> HasPositionVelocity a
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
        }


initBullet : Float -> Bullet
initBullet i =
    { position = vec -200 (i * 10)
    , velocity = vec 10 10
    , radius = 5
    , color = white
    }


updateBullet : Computer -> Bullet -> Bullet
updateBullet c bullet =
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
            { model | velocity = velocity }

        applyVelocity : HasPositionVelocity a -> HasPositionVelocity a
        applyVelocity model =
            { model | position = integrate model.position model.velocity }
    in
    bullet |> bounceWithinScreen c.screen |> applyVelocity


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
    , bullets : List Bullet
    , ticks : Int
    }


initialMemory : Memory
initialMemory =
    { player = initPlayer
    , turret = initTurret
    , bullets = []
    , ticks = 0
    }


addBulletEveryXSeconds seconds ticks list =
    let
        bulletCount =
            List.length list

        maxBullets =
            10

        period =
            modBy (seconds * 60) ticks

        shouldAddBullet =
            period == 0 && bulletCount < maxBullets
    in
    if shouldAddBullet then
        initBullet (toFloat bulletCount) :: list

    else
        list


update : Computer -> Memory -> Memory
update c model =
    let
        bullets =
            List.map (updateBullet c) model.bullets
                |> addBulletEveryXSeconds 1 model.ticks
    in
    { model
        | player = updatePlayer c model.player
        , bullets = bullets
        , ticks = model.ticks + 1
    }


view : Computer -> Memory -> List Shape
view _ model =
    renderPlayer model.player
        :: renderTurret model.turret
        :: List.map renderBullet model.bullets


main : Game Memory
main =
    game initialMemory update view
