module GravitronV3.Turret exposing
    ( Turret
    , TurretKind(..)
    , hit
    , initTurret
    , isDead
    , turretDeathResponse
    , turretStepResponse
    , turretToShape
    )

import GravitronV3.Bullet as Bullet exposing (Bullet, BulletKind(..))
import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Explosion as Explosion exposing (Explosion)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody
    exposing
        ( Circular
        , CircularBody
        , RigidBody
        )
import GravitronV3.Vec as Vec exposing (Vec)
import PointFree as PF



-- Turret Config


type TurretKind
    = GravityShooter1HP
    | GravityShooter2HP
    | TripleGravityShooter
    | GravityShooterOnDeathShoot5
    | HomingShooter
    | TimeBombShooter


turretKindToConfig : TurretKind -> TurretConfig
turretKindToConfig kind =
    let
        color =
            case kind of
                GravityShooter1HP ->
                    "red"

                GravityShooter2HP ->
                    "blue"

                TripleGravityShooter ->
                    "green"

                GravityShooterOnDeathShoot5 ->
                    "purple"

                HomingShooter ->
                    "orange"

                TimeBombShooter ->
                    "deeppink"

        maxHp =
            case kind of
                GravityShooter1HP ->
                    1

                GravityShooter2HP ->
                    2

                TripleGravityShooter ->
                    3

                GravityShooterOnDeathShoot5 ->
                    2

                HomingShooter ->
                    4

                TimeBombShooter ->
                    3

        ( bulletKind, bulletCount ) =
            case kind of
                GravityShooter1HP ->
                    ( GravityBullet, SingleBullet )

                GravityShooter2HP ->
                    ( GravityBullet, SingleBullet )

                TripleGravityShooter ->
                    ( GravityBullet, TripleBullets )

                GravityShooterOnDeathShoot5 ->
                    ( GravityBullet, SingleBullet )

                HomingShooter ->
                    ( HomingBullet, SingleBullet )

                TimeBombShooter ->
                    ( TimeBombBullet, SingleBullet )
    in
    { maxHP = maxHp
    , color = color
    , revengeOnDeath = kind == GravityShooterOnDeathShoot5
    , bulletKind = bulletKind
    , bulletCount = bulletCount
    }


type BulletCount
    = SingleBullet
    | TripleBullets
    | FiveBullets


type alias HP =
    { current : Int, max : Int }


type alias TurretConfig =
    { maxHP : Int
    , color : String
    , revengeOnDeath : Bool
    , bulletKind : BulletKind
    , bulletCount : BulletCount
    }



-- Turret


type alias Turret =
    { position : Point
    , velocity : Vec
    , radius : Float
    , bulletTimer : Counter
    , hp : HP
    , bulletKind : BulletKind
    , bulletCount : BulletCount
    , color : String
    , revengeOnDeath : Bool
    }


initHP : Int -> HP
initHP maxHP =
    HP maxHP maxHP


initTurret : Point -> TurretKind -> Turret
initTurret position kind =
    let
        config =
            turretKindToConfig kind
    in
    { position = position
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Counter.init 60
    , hp = initHP config.maxHP
    , bulletKind = config.bulletKind
    , bulletCount = config.bulletCount
    , color = config.color
    , revengeOnDeath = config.revengeOnDeath
    }


type alias HasHP a =
    { a | hp : HP }


isDead : HasHP a -> Bool
isDead { hp } =
    hp.current <= 0


hit : HasHP a -> HasHP a
hit hasHP =
    let
        decCurrentHP hp =
            { hp | current = max 0 (hp.current - 1) }
    in
    { hasHP | hp = decCurrentHP hasHP.hp }


hpPct : HP -> Float
hpPct hp =
    toFloat hp.current / toFloat hp.max


fireWeaponFromTo : CircularBody a -> Point -> ( BulletKind, BulletCount ) -> List Bullet
fireWeaponFromTo src target ( bulletKind, bulletCount ) =
    let
        addBulletWithAngle : Float -> Bullet
        addBulletWithAngle angle =
            Bullet.initBullet bulletKind src angle
    in
    let
        angle =
            Pt.vecFromTo src.position target
                |> Vec.angle
    in
    case bulletCount of
        SingleBullet ->
            [ addBulletWithAngle angle ]

        TripleBullets ->
            let
                angleSpread =
                    turns (1 / 8)
            in
            [ angle - angleSpread, angle, angle + angleSpread ]
                |> List.map addBulletWithAngle

        FiveBullets ->
            splitTurnInto 5
                |> List.map addBulletWithAngle


splitTurnInto : Int -> List Float
splitTurnInto parts =
    if parts <= 0 then
        []

    else
        let
            angleFrac =
                turns (1 / toFloat parts)
        in
        List.range 1 parts
            |> List.map ((+) -1 >> toFloat >> (*) angleFrac)


turretDeathResponse : Point -> Turret -> ( Explosion, List Bullet )
turretDeathResponse target turret =
    let
        addTurretExplosion =
            Explosion.explosionFrom turretToShape turret
    in
    case turret.revengeOnDeath of
        False ->
            ( addTurretExplosion, [] )

        True ->
            ( addTurretExplosion
            , fireWeaponFromTo turret target ( GravityBullet, FiveBullets )
            )


turretStepResponse : Point -> Turret -> ( Turret, List Bullet )
turretStepResponse target turret =
    let
        ( isDone, bulletTimer ) =
            Counter.cycleStep turret.bulletTimer

        addTurretResponse =
            { turret | bulletTimer = bulletTimer }
    in
    if isDone then
        ( addTurretResponse
        , fireWeaponFromTo turret target ( turret.bulletKind, turret.bulletCount )
        )

    else
        ( addTurretResponse, [] )


turretToShape : Turret -> Shape
turretToShape { radius, hp, color } =
    let
        fullShape =
            group
                [ circle radius
                    |> fill color
                    |> fade 0.7
                ]
    in
    group
        [ fullShape |> fade 0.9
        , fullShape |> scale (hpPct hp)
        ]
