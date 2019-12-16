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


type Weapon
    = BulletWeapon
    | TimeBombWeapon


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



--  Update


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen } ({ turrets, player } as mem) =
    mem
        |> stepExplosions
        |> stepBlastsToExplosions
        |> stepExpiredTimeBombsToBlasts
        |> stepTimeBombCollision
        |> stepBulletCollision
        |> stepTimeBombsVel player.x player.y
        |> stepTimeBombsPos
        |> stepBounceTimeBombInScreen screen
        |> stepPlayerPosition time
        |> stepBulletsVel player.x player.y
        |> stepBulletsPos
        |> stepBounceBulletInScreen screen
        |> stepFireTurretWeapon player.x player.y


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


stepExplosions : Mem -> Mem
stepExplosions mem =
    let
        stepE e =
            if isDone e.ct then
                Nothing

            else
                Just { e | ct = stepCt e.ct }
    in
    { mem | explosions = List.filterMap stepE mem.explosions }


stepBlastsToExplosions : Mem -> Mem
stepBlastsToExplosions mem =
    let
        toExplosion { x, y, r } =
            initExplosion x y r red
    in
    { mem
        | explosions = List.map toExplosion mem.blasts ++ mem.explosions
        , blasts = []
    }


stepExpiredTimeBombsToBlasts : Mem -> Mem
stepExpiredTimeBombsToBlasts mem =
    let
        reducer tb acc =
            if isDone tb.ct then
                { acc
                    | blasts = blastFromTimeBomb tb :: acc.blasts
                }

            else
                { acc | timeBombs = { tb | ct = stepCt tb.ct } :: acc.timeBombs }
    in
    List.foldl reducer { mem | timeBombs = [] } mem.timeBombs


blastFromTimeBomb : TimeBomb -> Blast
blastFromTimeBomb tb =
    Blast tb.x tb.y timeBombBlastRad


stepTimeBombCollision : Mem -> Mem
stepTimeBombCollision =
    let
        bbc : TimeBomb -> TimeBomb -> Bool
        bbc b ob =
            ccc b.x b.y bRad ob.x ob.y bRad

        tbBulletC tb b =
            ccc tb.x tb.y bRad b.x b.y bRad

        selfC ( tb, tbList ) mem =
            if List.any (bbc tb) tbList || List.any (tbBulletC tb) mem.bullets then
                { mem | blasts = blastFromTimeBomb tb :: mem.blasts }

            else
                { mem | timeBombs = tb :: mem.timeBombs }
    in
    \mem ->
        mem.timeBombs
            |> List.Extra.select
            |> List.foldl selfC { mem | timeBombs = [] }


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


stepFireTurretWeapon : Float -> Float -> Mem -> Mem
stepFireTurretWeapon x y =
    let
        fireWeaponOnCounter t mem =
            if isDone t.ct then
                let
                    angle =
                        angleFromTo t.x t.y x y
                in
                case t.weapon of
                    BulletWeapon ->
                        { mem | bullets = initBullet t.x t.y 3 angle :: mem.bullets }

                    TimeBombWeapon ->
                        { mem | timeBombs = initTimeBomb t.x t.y 3 angle :: mem.timeBombs }

            else
                mem

        stepTurretCounters : Mem -> Mem
        stepTurretCounters mem =
            let
                stepTurret : Turret -> Turret
                stepTurret t =
                    { t | ct = stepCt t.ct }
            in
            { mem | turrets = List.map stepTurret mem.turrets }
    in
    \mem ->
        List.foldl fireWeaponOnCounter mem mem.turrets
            |> stepTurretCounters



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
                |> fade (0.7 - (progress * 0.7))
                |> scale (1 + (progress / 4))
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
