module GravitronV2.Bullet exposing
    ( Bullet
    , BulletType(..)
    , defaultBullet
    , renderBullet
    , stepBullet
    )

-- Bullet

import Color
import GravitronV2.Game as G exposing (Shape)
import GravitronV2.HasHealth as HasHealth
import GravitronV2.Particle as Particle exposing (HasPosition)
import GravitronV2.Timer as Timer exposing (Timer)
import GravitronV2.Vec as V exposing (Vec, vec)
import PointFree exposing (when)
import TypedSvg
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as InPx


type alias Bullet =
    { position : Vec
    , velocity : Vec
    , maxSpeed : Float
    , radius : Float
    , color : G.Color
    , health : HasHealth.Health
    , bulletType : BulletType
    }


type BulletType
    = GravityBullet
    | HomingBullet
    | TimeBombBullet Timer


init : Vec -> Vec -> Float -> BulletType -> Bullet
init from to offset type_ =
    let
        bullet =
            defaultBullet

        angle =
            V.fromPt from to |> V.angle
    in
    { bullet
        | position =
            V.fromRTheta (offset + bullet.radius + 1) angle
                |> V.add from
        , velocity = V.fromRTheta (V.len bullet.velocity) angle
        , bulletType = type_
    }


defaultBullet : Bullet
defaultBullet =
    let
        speed =
            2.8

        maxSpeed =
            7
    in
    { position = V.zero
    , velocity = vec speed speed
    , maxSpeed = maxSpeed
    , radius = 5
    , color = G.white
    , health = HasHealth.init 1
    , bulletType = GravityBullet
    }


bounceWithinScreen : G.Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreen screen position bounceFactor velocity =
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
        newBouncedVelocity |> V.mapMagnitude ((*) bounceFactor)

    else
        newBouncedVelocity


stepBullet : Float -> G.Screen -> HasPosition a -> Bullet -> Bullet
stepBullet rTicks screen target bullet =
    let
        applyAccForce =
            case bullet.bulletType of
                GravityBullet ->
                    let
                        gravityVec =
                            V.fromPt bullet.position target.position
                                |> V.mapMagnitude (\m -> 20 / m)
                    in
                    V.add gravityVec

                HomingBullet ->
                    let
                        homingVec =
                            V.fromPt bullet.position target.position
                                |> V.mapMagnitude (always 0.3)
                    in
                    V.add homingVec
                        >> V.mapMagnitude ((*) 0.98)

                TimeBombBullet _ ->
                    let
                        gravityVec =
                            V.fromPt bullet.position target.position
                                |> V.mapMagnitude (\m -> 20 / m)
                    in
                    V.add gravityVec
    in
    bullet
        |> Particle.mapVelocity
            (identity
                >> bounceWithinScreen screen bullet.position 0.5
                >> applyAccForce
            )
        |> Particle.step
        |> when (timeBombFired rTicks) HasHealth.kill


timeBombFired : Float -> Bullet -> Bool
timeBombFired rTicks bullet =
    case bullet.bulletType of
        TimeBombBullet timer ->
            Timer.isDone rTicks timer

        _ ->
            False


renderBulletHelp : Float -> Bullet -> List Shape
renderBulletHelp rTicks bullet =
    let
        ( x, y ) =
            ( 0, 0 )

        position =
            V.zero

        simpleBulletCircle =
            G.circleAt x y bullet.radius bullet.color
    in
    case bullet.bulletType of
        GravityBullet ->
            [ simpleBulletCircle ]

        HomingBullet ->
            let
                angle =
                    V.angle bullet.velocity

                extRadius =
                    bullet.radius * 2

                extVec =
                    V.fromRTheta extRadius angle

                ( x1, y1 ) =
                    V.add position extVec |> V.toTuple

                ( x2, y2 ) =
                    V.add position (V.scaleBy -1 extVec) |> V.toTuple
            in
            [ simpleBulletCircle
            , G.customShape
                (TypedSvg.line
                    [ InPx.x1 x1
                    , InPx.y1 y1
                    , InPx.x2 x2
                    , InPx.y2 y2
                    , TA.stroke Color.white
                    ]
                    []
                )
            ]

        TimeBombBullet bombTimer ->
            [ simpleBulletCircle
            , let
                progressArcRadius =
                    bullet.radius + bullet.radius / 2

                progress =
                    Timer.value rTicks bombTimer

                xOffset =
                    progressArcRadius
              in
              if progress > 0 then
                G.strokeArc ( x, y )
                    (turns progress)
                    ( x + xOffset, y )
                    G.green

              else
                G.noShape
            ]


renderBullet : Float -> Bullet -> Shape
renderBullet rTicks bullet =
    let
        ( x, y ) =
            V.toTuple bullet.position
    in
    renderBulletHelp rTicks bullet
        |> G.group
        |> G.move x y
