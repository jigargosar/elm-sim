module GravitronV4.Main exposing (main)

import List.Extra
import Playground exposing (..)


type Counter
    = Counter Int Int


initCt : Int -> Counter
initCt =
    Counter 0 << max 1


stepCt : Counter -> Counter
stepCt (Counter n mx) =
    Counter (n + 1 |> modBy mx) mx


isDone : Counter -> Bool
isDone (Counter n mx) =
    n == mx - 1


ctProgress : Counter -> Float
ctProgress (Counter n mx) =
    if n == 0 then
        0

    else
        toFloat n / toFloat mx



-- Model


type alias Player =
    { x : Number
    , y : Number
    }


type alias Turret =
    { ct : Counter
    , x : Number
    , y : Number
    , color : Color
    , weapon : Weapon
    }


type alias Bullet =
    { x : Number
    , y : Number
    , vx : Number
    , vy : Number
    }


initBullet : Number -> Number -> Number -> Number -> Bullet
initBullet x y speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )
    in
    Bullet x y vx vy


type alias TimeBomb =
    { x : Number
    , y : Number
    , vx : Number
    , vy : Number
    , ct : Counter
    }


initTimeBomb : Number -> Number -> Number -> Number -> TimeBomb
initTimeBomb x y speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )
    in
    TimeBomb x y vx vy (initCt (60 * 2))


type alias Blast =
    { x : Number
    , y : Number
    , r : Number
    }


type alias Explosion =
    { ct : Counter
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    }


initExplosion : Number -> Number -> Number -> Color -> Explosion
initExplosion =
    Explosion (initCt 60)


type alias Mem =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , timeBombs : List TimeBomb
    , blasts : List Blast
    , explosions : List Explosion
    }


initialMemory : Mem
initialMemory =
    { player = Player 0 0
    , turrets = initTurrets [ ( red, BulletWeapon ), ( red, BulletWeapon ), ( blue, TimeBombWeapon ), ( orange, BulletWeapon ) ]
    , bullets = []
    , timeBombs = []
    , blasts = []
    , explosions = []
    }


initTurrets : List ( Color, Weapon ) -> List Turret
initTurrets =
    let
        positions =
            [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ]

        factor =
            150

        initTurret ( x, y ) ( c, w ) =
            Turret (initCt 60) (x * factor) (y * factor) c w
    in
    List.map2 initTurret positions



--  Update


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen } ({ turrets, player } as mem) =
    mem
        |> stepBlastsToExplosions
        |> stepExpiredTimeBombsToBlasts
        |> stepTimeBombCollision
        |> stepTimeBombsVel player.x player.y
        |> stepTimeBombsPos
        |> stepBounceTimeBombInScreen screen
        |> stepPlayerPosition time
        |> stepBulletCollision
        |> stepBulletsVel player.x player.y
        |> stepBulletsPos
        |> stepBounceBulletInScreen screen
        |> stepTimeBombCounters
        |> stepFireTurretWeapon player.x player.y
        |> stepTurretCounters


stepPlayerPosition : Time -> Mem -> Mem
stepPlayerPosition time mem =
    let
        stepPlayerPosition_ p =
            { p | x = wave -100 100 11 time, y = wave -300 300 5 time }
    in
    { mem | player = stepPlayerPosition_ mem.player }


stepBulletCollision : Mem -> Mem
stepBulletCollision mem =
    let
        bbc : Bullet -> Bullet -> Bool
        bbc b ob =
            ccc b.x b.y bRad ob.x ob.y bRad

        bbLstC : ( Bullet, List Bullet ) -> Maybe Bullet
        bbLstC ( b, bLst ) =
            if List.any (bbc b) bLst then
                Nothing

            else
                Just b
    in
    { mem
        | bullets =
            List.Extra.select mem.bullets
                |> List.filterMap bbLstC
    }


stepBulletsPos : Mem -> Mem
stepBulletsPos mem =
    let
        stepBullet : Bullet -> Bullet
        stepBullet b =
            { b | x = b.x + b.vx, y = b.y + b.vy }
    in
    { mem | bullets = List.map stepBullet mem.bullets }


stepBulletsVel : Number -> Number -> Mem -> Mem
stepBulletsVel tx ty mem =
    let
        stepBullet : Bullet -> Bullet
        stepBullet b =
            let
                ( dx, dy ) =
                    ( tx - b.x, ty - b.y )
                        |> toPolar
                        |> Tuple.mapFirst (\m -> 20 / m)
                        |> fromPolar
            in
            { b | vx = b.vx + dx, vy = b.vy + dy }
    in
    { mem | bullets = List.map stepBullet mem.bullets }


stepBounceBulletInScreen : Screen -> Mem -> Mem
stepBounceBulletInScreen scr mem =
    let
        bounce ({ x, y, vx, vy } as b) =
            let
                ( nvx, nvy ) =
                    bounceInScreen 0.5 scr x y vx vy
            in
            { b | vx = nvx, vy = nvy }
    in
    { mem | bullets = List.map bounce mem.bullets }


stepBlastsToExplosions : Mem -> Mem
stepBlastsToExplosions mem =
    let
        toExplosion { x, y, r } =
            initExplosion x y r red
    in
    { mem
        | explosions = List.map toExplosion mem.blasts
        , blasts = []
    }


stepExpiredTimeBombsToBlasts : Mem -> Mem
stepExpiredTimeBombsToBlasts mem =
    let
        reducer tb acc =
            if isDone tb.ct then
                { acc
                    | blasts = Blast tb.x tb.y timeBombBlastRad :: acc.blasts
                }

            else
                { acc | timeBombs = tb :: acc.timeBombs }
    in
    List.foldl reducer { mem | timeBombs = [] } mem.timeBombs


stepTimeBombCollision : Mem -> Mem
stepTimeBombCollision mem =
    let
        bbc : TimeBomb -> TimeBomb -> Bool
        bbc b ob =
            ccc b.x b.y bRad ob.x ob.y bRad

        bbLstC : ( TimeBomb, List TimeBomb ) -> Maybe TimeBomb
        bbLstC ( b, bLst ) =
            if List.any (bbc b) bLst then
                Nothing

            else
                Just b
    in
    { mem
        | timeBombs =
            mem.timeBombs
                |> List.Extra.select
                |> List.filterMap bbLstC
    }


stepTimeBombsPos : Mem -> Mem
stepTimeBombsPos mem =
    let
        stepTimeBomb : TimeBomb -> TimeBomb
        stepTimeBomb b =
            { b | x = b.x + b.vx, y = b.y + b.vy }
    in
    { mem | timeBombs = List.map stepTimeBomb mem.timeBombs }


stepTimeBombsVel : Number -> Number -> Mem -> Mem
stepTimeBombsVel tx ty mem =
    let
        stepTimeBomb : TimeBomb -> TimeBomb
        stepTimeBomb b =
            let
                ( dx, dy ) =
                    ( tx - b.x, ty - b.y )
                        |> toPolar
                        |> Tuple.mapFirst (\m -> 20 / m)
                        |> fromPolar
            in
            { b | vx = b.vx + dx, vy = b.vy + dy }
    in
    { mem | timeBombs = List.map stepTimeBomb mem.timeBombs }


stepBounceTimeBombInScreen : Screen -> Mem -> Mem
stepBounceTimeBombInScreen scr mem =
    let
        bounce ({ x, y, vx, vy } as b) =
            let
                ( nvx, nvy ) =
                    bounceInScreen 0.5 scr x y vx vy
            in
            { b | vx = nvx, vy = nvy }
    in
    { mem | timeBombs = List.map bounce mem.timeBombs }


stepTimeBombCounters : Mem -> Mem
stepTimeBombCounters mem =
    let
        stepTimeBomb : TimeBomb -> TimeBomb
        stepTimeBomb t =
            { t | ct = stepCt t.ct }
    in
    { mem | timeBombs = List.map stepTimeBomb mem.timeBombs }


stepTurretCounters : Mem -> Mem
stepTurretCounters mem =
    let
        stepTurret : Turret -> Turret
        stepTurret t =
            { t | ct = stepCt t.ct }
    in
    { mem | turrets = List.map stepTurret mem.turrets }


type Weapon
    = BulletWeapon
    | TimeBombWeapon


type Projectile
    = BulletProjectile Bullet
    | TimeBombProjectile TimeBomb


stepFireTurretWeapon : Float -> Float -> Mem -> Mem
stepFireTurretWeapon x y mem =
    let
        initProjectile : Turret -> Float -> Projectile
        initProjectile t angle =
            case t.weapon of
                BulletWeapon ->
                    BulletProjectile (initBullet t.x t.y 3 angle)

                TimeBombWeapon ->
                    TimeBombProjectile (initTimeBomb t.x t.y 3 angle)

        fireWeaponOnCounter t =
            if isDone t.ct then
                let
                    angle =
                        angleFromTo t.x t.y x y
                in
                (::) (initProjectile t angle)

            else
                identity

        projectiles : List Projectile
        projectiles =
            List.foldl fireWeaponOnCounter [] mem.turrets

        addProjectile : Projectile -> Mem -> Mem
        addProjectile p m =
            case p of
                BulletProjectile b ->
                    { m | bullets = b :: m.bullets }

                TimeBombProjectile timeBomb ->
                    { m | timeBombs = timeBomb :: m.timeBombs }
    in
    List.foldl addProjectile mem projectiles



-- View


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { player, turrets, bullets, timeBombs, explosions } =
    [ viewPlayer player
    , viewTurrets turrets
    , viewBullets bullets
    , viewTimeBombs timeBombs
    , viewExplosions explosions
    ]


viewPlayer : Player -> Shape
viewPlayer { x, y } =
    circle green 20
        |> move x y


viewTurrets : List Turret -> Shape
viewTurrets =
    let
        viewTurret { x, y, color } =
            circle color 25
                |> move x y
    in
    List.map viewTurret >> group


bRad : Float
bRad =
    6


viewBullets : List Bullet -> Shape
viewBullets =
    let
        viewBullet { x, y } =
            circle black bRad
                |> fade 0.8
                |> move x y
    in
    List.map viewBullet >> group


timeBombBlastRad =
    bRad * 10


viewTimeBombs : List TimeBomb -> Shape
viewTimeBombs =
    let
        viewTimeBomb : TimeBomb -> Shape
        viewTimeBomb { x, y } =
            group
                [ circle red bRad
                    |> fade 0.8
                , circle red timeBombBlastRad
                    |> fade 0.1
                ]
                |> move x y
    in
    List.map viewTimeBomb >> group


viewExplosions : List Explosion -> Shape
viewExplosions =
    let
        viewExplosion : Explosion -> Shape
        viewExplosion { ct, x, y, r, color } =
            let
                progress =
                    ctProgress ct
            in
            circle color r
                |> fade (1 - progress)
                |> scale (1 + (progress / 2))
                |> move x y
    in
    List.map viewExplosion >> group


main =
    Playground.game viewMemory updateMemory initialMemory



-- Geom Helpers


bounceInScreen :
    Float
    -> Screen
    -> Float
    -> Float
    -> Float
    -> Float
    -> ( Float, Float )
bounceInScreen bounceFriction scr x y vx vy =
    let
        nvx =
            if
                (x < scr.left && vx < 0)
                    || (x > scr.right && vx > 0)
            then
                negate vx

            else
                vx

        nvy =
            if
                (y < scr.bottom && vy < 0)
                    || (y > scr.top && vy > 0)
            then
                negate vy

            else
                vy
    in
    if nvx /= vx || nvy /= vy then
        toPolar ( nvx, nvy )
            |> Tuple.mapFirst ((*) bounceFriction)
            |> fromPolar

    else
        ( nvx, nvy )


ccc : Number -> Number -> Number -> Number -> Number -> Number -> Bool
ccc x y r x2 y2 r2 =
    ((x2 - x) ^ 2 + (y2 - y) ^ 2)
        < (r ^ 2 + r2 ^ 2)


angleFromTo : Float -> Float -> Float -> Float -> Float
angleFromTo x y x2 y2 =
    atan2 (y2 - y) (x2 - x)
