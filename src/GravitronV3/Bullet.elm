module GravitronV3.Bullet exposing (Bullet, BulletKind(..), bulletToShape, initBullet, isFakeBullet, stepBullet, toTaggedCircle)

import Basics.Extra exposing (inDegrees)
import GravitronV3.Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Explosion as Explosion exposing (Explosion)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody
    exposing
        ( Circular
        , CircularBody
        , RigidBody
        )
import GravitronV3.Screen exposing (Screen)
import GravitronV3.Tag as Tag
import GravitronV3.Vec as Vec exposing (Vec)


type BulletKind
    = GravityBullet
    | HomingBullet
    | TimeBombBullet


type BulletMotion
    = Gravity
    | Homing


type alias Bullet =
    CircularBody { motion : BulletMotion, timeBomb : Maybe Counter }


type alias BulletConfig =
    { motion : BulletMotion
    , timeBomb : Maybe Counter
    }


bulletKindToConfig : BulletKind -> BulletConfig
bulletKindToConfig kind =
    case kind of
        GravityBullet ->
            { motion = Gravity, timeBomb = Nothing }

        HomingBullet ->
            { motion = Gravity, timeBomb = Nothing }

        TimeBombBullet ->
            { motion = Gravity, timeBomb = Just <| Counter.init (60 * 2) }


initBullet : BulletKind -> Circular a -> Float -> Bullet
initBullet kind gun angle =
    let
        config =
            bulletKindToConfig kind

        radius =
            6

        speed =
            2
    in
    { position =
        Pt.moveBy
            (Vec.fromRTheta (radius + gun.radius) angle)
            gun.position
    , velocity = Vec.fromRTheta speed angle
    , radius = radius
    , motion = config.motion
    , timeBomb = config.timeBomb
    }


isFakeBullet : Bullet -> Bool
isFakeBullet bullet =
    bullet.timeBomb /= Nothing


toTaggedCircle : Bullet -> Tag.TaggedCircle
toTaggedCircle =
    Tag.circular Tag.Bullet


stepBullet : Screen -> { target | position : Point, radius : Float } -> Bullet -> Bullet
stepBullet screen target bullet =
    bullet
        |> RigidBody.step
            [ case bullet.motion of
                Gravity ->
                    gravitateTo target

                Homing ->
                    homingTo target
            , bounceWithinScreen screen 0.5
            ]


bulletToShape : Bullet -> Shape
bulletToShape bullet =
    let
        { radius, motion, velocity } =
            bullet

        otherShape =
            case motion of
                Gravity ->
                    group []

                Homing ->
                    group
                        [ rect (radius * 3.5) 1
                            |> rotate (Vec.angle velocity |> inDegrees)
                        ]
    in
    group
        [ group
            [ circle radius
            , otherShape
            ]
            |> fill "black"
            |> fade 0.7
        ]


bounceWithinScreenHelp : Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreenHelp screen position bounceFactor velocity =
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
            Vec.toTuple position

        ( vx, vy ) =
            Vec.toTuple velocity

        newBouncedVelocity =
            Vec.vec (bounceVelocityPart screen.left screen.right x vx)
                (bounceVelocityPart screen.top screen.bottom y vy)
    in
    if velocity /= newBouncedVelocity then
        newBouncedVelocity |> Vec.mapMagnitude ((*) bounceFactor)

    else
        newBouncedVelocity


bounceWithinScreen : Screen -> Float -> RigidBody a -> Vec
bounceWithinScreen screen factor m =
    bounceWithinScreenHelp screen
        (m.position |> (Pt.toTuple >> Vec.fromTuple))
        factor
        m.velocity


gravitateTo : Circular target -> RigidBody model -> Vec
gravitateTo target model =
    model.velocity
        |> Vec.add
            (Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (\mag -> 20 / mag)
            )


homingTo : Circular target -> RigidBody model -> Vec
homingTo target model =
    let
        homingVec =
            Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (always 0.3)
    in
    model.velocity
        |> Vec.add homingVec
        |> Vec.mapMagnitude ((*) 0.98)
