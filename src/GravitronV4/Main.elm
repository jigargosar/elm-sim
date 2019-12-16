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


type alias Player =
    { x : Number
    , y : Number
    }


type alias Turret =
    { ct : Counter
    , x : Number
    , y : Number
    , color : Color
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


type alias Mem =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    }


initialMemory : Mem
initialMemory =
    { player = Player 0 0
    , turrets = initTurrets [ red, red, blue, orange ]
    , bullets = []
    }


initTurrets : List Color -> List Turret
initTurrets =
    let
        positions =
            [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ]

        factor =
            150

        initTurret ( x, y ) =
            Turret (initCt 60) (x * factor) (y * factor)
    in
    List.map2 initTurret positions



--  Update


updateMemory : Computer -> Mem -> Mem
updateMemory { screen } ({ turrets, player } as mem) =
    mem
        |> stepBulletCollision
        |> stepBulletsVel player.x player.y
        |> stepBulletsPos
        |> stepBounceBulletInScreen screen
        |> stepFireTurretBullets player.x player.y
        |> stepTurretCounters


ccc : Number -> Number -> Number -> Number -> Number -> Number -> Bool
ccc x y r x2 y2 r2 =
    ((x2 - x) ^ 2 + (y2 - y) ^ 2)
        < (r ^ 2 + r2 ^ 2)


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
    { mem | bullets = List.Extra.select mem.bullets |> List.filterMap bbLstC }


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


stepTurretCounters : Mem -> Mem
stepTurretCounters mem =
    let
        stepTurret : Turret -> Turret
        stepTurret t =
            { t | ct = stepCt t.ct }
    in
    { mem | turrets = List.map stepTurret mem.turrets }


stepFireTurretBullets : Float -> Float -> Mem -> Mem
stepFireTurretBullets x y mem =
    let
        fireBulletOnCounter t =
            if isDone t.ct then
                let
                    angle =
                        atan2 (y - t.y) (x - t.x)
                in
                (::) (initBullet t.x t.y 1 angle)

            else
                identity
    in
    { mem
        | bullets =
            (List.foldl fireBulletOnCounter [] mem.turrets
                ++ mem.bullets
            )
                |> List.take 20
    }



-- View


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { player, turrets, bullets } =
    [ viewPlayer player
    , viewTurrets turrets
    , viewBullets bullets
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
