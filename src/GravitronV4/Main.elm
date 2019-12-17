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


type Id
    = PlayerId
    | BulletId Int
    | TurretId Int
    | TimeBombId Int
    | ExplosionId Int
    | BlastId Int


type alias Player =
    { id : Id
    , x : Number
    , y : Number
    }


initPlayer : Number -> Number -> Player
initPlayer =
    Player PlayerId


playerRadius : Float
playerRadius =
    20


type alias Turret =
    { id : Id
    , ct : Counter
    , x : Number
    , y : Number
    , color : Color
    , weapon : Weapon
    }


type Weapon
    = BulletWeapon
    | TimeBombWeapon


initTurrets : List ( Id, Color, Weapon ) -> List Turret
initTurrets =
    let
        positions =
            [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ]

        factor =
            150

        initTurret ( x, y ) ( id, c, w ) =
            Turret id (initCt 160) (x * factor) (y * factor) c w
    in
    List.map2 initTurret positions


type alias Bullet =
    { id : Id
    , x : Number
    , y : Number
    , vx : Number
    , vy : Number
    }


bulletRadius : Float
bulletRadius =
    6


initBullet : Id -> Number -> Number -> Number -> Number -> Bullet
initBullet id x y speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )
    in
    Bullet id x y vx vy


type alias TimeBomb =
    { id : Id
    , x : Number
    , y : Number
    , vx : Number
    , vy : Number
    , ct : Counter
    }


initTimeBomb : Id -> Number -> Number -> Number -> Number -> TimeBomb
initTimeBomb id x y speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )
    in
    TimeBomb id x y vx vy (initCt (60 * 2))


timeBombRadius : Float
timeBombRadius =
    bulletRadius


timeBombBlastRadius : Float
timeBombBlastRadius =
    bulletRadius * 10


type alias Blast =
    { id : Id
    , x : Number
    , y : Number
    , r : Number
    }


initBlast : Id -> Number -> Number -> Number -> Blast
initBlast id x y r =
    Blast id x y r


type alias Explosion =
    { id : Id
    , ct : Counter
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    }


initExplosion : Id -> Number -> Number -> Number -> Color -> Explosion
initExplosion id =
    Explosion id (initCt 60)


type alias Mem =
    { nextId : Int
    , player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , timeBombs : List TimeBomb
    , blasts : List Blast
    , explosions : List Explosion
    }


initialMemory : Mem
initialMemory =
    { nextId = 100
    , player = initPlayer 0 0
    , turrets =
        initTurrets
            [ ( TurretId 0, red, BulletWeapon )
            , ( TurretId 1, red, BulletWeapon )
            , ( TurretId 2, blue, TimeBombWeapon )
            , ( TurretId 3, orange, BulletWeapon )
            ]
    , bullets = []
    , timeBombs = []
    , blasts = []
    , explosions = []
    }



--  Update


type Tag
    = TagTimeBomb
    | TagBullet
    | TagBlast
    | TagPlayer


playerToDamageCircle : Player -> DamageCircle
playerToDamageCircle { x, y } =
    DamageCircle x y playerRadius TagPlayer [ TagBullet, TagTimeBomb ]


blastToDamageCircle : Blast -> DamageCircle
blastToDamageCircle { x, y, r } =
    DamageCircle x y r TagBlast [ TagTimeBomb, TagBullet ]


bulletToDamageCircle : Bullet -> DamageCircle
bulletToDamageCircle { x, y } =
    DamageCircle x y bulletRadius TagBullet [ TagTimeBomb, TagBullet ]


timeBombToDamageCircle : TimeBomb -> DamageCircle
timeBombToDamageCircle { x, y } =
    DamageCircle x y timeBombRadius TagTimeBomb [ TagTimeBomb ]


type alias DamageCircle =
    { x : Number
    , y : Number
    , r : Number
    , tag : Tag
    , canDamage : List Tag
    }


isDamaging : DamageCircle -> DamageCircle -> Bool
isDamaging target src =
    let
        dcc : DamageCircle -> DamageCircle -> Bool
        dcc a b =
            ccc a.x a.y a.r b.x b.y b.r
    in
    List.member target.tag src.canDamage
        && dcc target src


type Res
    = AddExplosion Explosion
    | NewExplosion Number Number Number Color
    | AddBlast Blast
    | NewBlast Number Number Number
    | AddTurret Turret
    | AddBullet Bullet
    | NewBullet Number Number Number Number
    | AddTimeBomb TimeBomb
    | NewTimeBomb Number Number Number Number
    | NoRes
    | Batch (List Res)


newExplosion : Number -> Number -> Number -> Color -> Res
newExplosion =
    NewExplosion


newBlast : Number -> Number -> Number -> Res
newBlast =
    NewBlast


foldRes : List Res -> Mem -> Mem
foldRes resList =
    let
        reducer : Res -> Mem -> Mem
        reducer res ({ nextId, explosions, blasts, turrets, bullets, timeBombs } as mem) =
            case res of
                AddExplosion explosion ->
                    { mem | explosions = explosion :: explosions }

                AddBlast blast ->
                    { mem | blasts = blast :: blasts }

                AddTurret turret ->
                    { mem | turrets = turret :: turrets }

                AddBullet bullet ->
                    { mem | bullets = bullet :: bullets }

                AddTimeBomb timeBomb ->
                    { mem | timeBombs = timeBomb :: timeBombs }

                NewBullet x y speed angle ->
                    reducer
                        (AddBullet (initBullet (BulletId nextId) x y speed angle))
                        { mem | nextId = nextId + 1 }

                NewTimeBomb x y speed angle ->
                    reducer
                        (AddTimeBomb (initTimeBomb (TimeBombId nextId) x y speed angle))
                        { mem | nextId = nextId + 1 }

                NewExplosion x y r c ->
                    reducer
                        (AddExplosion (initExplosion (ExplosionId nextId) x y r c))
                        { mem | nextId = nextId + 1 }

                NewBlast x y r ->
                    reducer
                        (AddBlast (initBlast (BlastId nextId) x y r))
                        { mem | nextId = nextId + 1 }

                NoRes ->
                    mem

                Batch lst ->
                    foldRes lst mem
    in
    \mem -> List.foldl reducer mem resList


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen } mem =
    let
        { turrets, player, explosions, blasts, bullets, timeBombs } =
            mem

        env =
            { scr = screen
            , tx = player.x
            , ty = player.y
            , player = playerToDamageCircle player
            , blasts = List.map blastToDamageCircle blasts
            , bullets = List.map bulletToDamageCircle bullets
            }
    in
    { mem
        | player = updatePlayer time player
        , explosions = []
        , blasts = []
        , turrets = []
        , bullets = []
        , timeBombs = []
    }
        |> foldRes (stepBlasts blasts)
        |> foldRes (stepExplosions explosions)
        |> foldRes (stepTurrets env turrets)
        |> foldRes (stepBullets env bullets)
        |> foldRes (stepTimeBombs env timeBombs)



{- |> stepExpiredTimeBombsToBlasts
   |> stepTimeBombCollisionToBlasts
   |> stepBulletCollision
   |> stepTimeBombsVel player.x player.y
   |> stepTimeBombsPos
   |> stepBounceTimeBombInScreen screen
   |> stepBulletsVel player.x player.y
   |> stepBulletsPos
   |> stepBounceBulletInScreen screen
   |> stepFireTurretWeapon player.x player.y
-}


updatePlayer : Time -> Player -> Player
updatePlayer time p =
    { p | x = wave -100 100 11 time, y = wave -300 300 5 time }


stepTimeBombs :
    { a
        | scr : Screen
        , tx : Float
        , ty : Float
        , player : DamageCircle
        , blasts : List DamageCircle
        , bullets : List DamageCircle
    }
    -> List TimeBomb
    -> List Res
stepTimeBombs { scr, tx, ty, player, blasts, bullets } =
    let
        updateVel : TimeBomb -> TimeBomb
        updateVel b =
            let
                ( dx, dy ) =
                    ( tx - b.x, ty - b.y )
                        |> toPolar
                        |> Tuple.mapFirst (\m -> 20 / m)
                        |> fromPolar
            in
            { b | vx = b.vx + dx, vy = b.vy + dy }

        updatePos : TimeBomb -> TimeBomb
        updatePos b =
            { b | x = b.x + b.vx, y = b.y + b.vy }

        bounce : TimeBomb -> TimeBomb
        bounce ({ x, y, vx, vy } as b) =
            let
                ( nvx, nvy ) =
                    bounceInScreen 0.5 scr x y vx vy
            in
            { b | vx = nvx, vy = nvy }

        updateBombClock : TimeBomb -> TimeBomb
        updateBombClock b =
            { b | ct = stepCt b.ct }

        stepAlive : TimeBomb -> Res
        stepAlive =
            updateVel
                >> updatePos
                >> bounce
                >> updateBombClock
                >> AddTimeBomb

        stepDead { x, y } =
            Batch
                [ newExplosion x y timeBombRadius red
                , newBlast x y timeBombBlastRadius
                ]

        step : ( TimeBomb, List TimeBomb ) -> Res
        step ( timeBomb, otherTimeBombs ) =
            if
                isDone timeBomb.ct
                    || List.any (isDamaging (timeBombToDamageCircle timeBomb))
                        (player
                            :: List.map timeBombToDamageCircle otherTimeBombs
                            ++ blasts
                            ++ bullets
                        )
            then
                stepDead timeBomb

            else
                stepAlive timeBomb
    in
    List.Extra.select >> List.map step


stepBullets :
    { a
        | scr : Screen
        , tx : Float
        , ty : Float
        , player : DamageCircle
        , blasts : List DamageCircle
    }
    -> List Bullet
    -> List Res
stepBullets { scr, tx, ty, player, blasts } =
    let
        updateVel : Bullet -> Bullet
        updateVel b =
            let
                ( dx, dy ) =
                    ( tx - b.x, ty - b.y )
                        |> toPolar
                        |> Tuple.mapFirst (\m -> 20 / m)
                        |> fromPolar
            in
            { b | vx = b.vx + dx, vy = b.vy + dy }

        updatePos : Bullet -> Bullet
        updatePos b =
            { b | x = b.x + b.vx, y = b.y + b.vy }

        bounce : Bullet -> Bullet
        bounce ({ x, y, vx, vy } as b) =
            let
                ( nvx, nvy ) =
                    bounceInScreen 0.5 scr x y vx vy
            in
            { b | vx = nvx, vy = nvy }

        stepAlive : Bullet -> Res
        stepAlive =
            updateVel >> updatePos >> bounce >> AddBullet

        stepDead { x, y } =
            newExplosion x y bulletRadius black

        step : ( Bullet, List Bullet ) -> Res
        step ( bullet, otherBullets ) =
            if
                List.any (isDamaging (bulletToDamageCircle bullet))
                    (player
                        :: List.map bulletToDamageCircle otherBullets
                        ++ blasts
                    )
            then
                stepDead bullet

            else
                stepAlive bullet
    in
    List.Extra.select >> List.map step


stepExplosions : List Explosion -> List Res
stepExplosions =
    let
        stepE e =
            if isDone e.ct then
                NoRes

            else
                AddExplosion { e | ct = stepCt e.ct }
    in
    List.map stepE


stepBlasts : List Blast -> List Res
stepBlasts =
    let
        toExplosion { x, y, r } =
            newExplosion x y r red
    in
    List.map toExplosion


stepTurrets : { a | tx : Float, ty : Float } -> List Turret -> List Res
stepTurrets { tx, ty } =
    let
        fireWeaponOnCounter t =
            [ if isDone t.ct then
                let
                    angle =
                        angleFromTo t.x t.y tx ty
                in
                case t.weapon of
                    BulletWeapon ->
                        NewBullet t.x t.y 3 angle

                    TimeBombWeapon ->
                        NewTimeBomb t.x t.y 3 angle

              else
                NoRes
            , AddTurret { t | ct = stepCt t.ct }
            ]
                |> Batch
    in
    List.map fireWeaponOnCounter



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
    circle green playerRadius
        |> move x y


viewTurrets : List Turret -> Shape
viewTurrets =
    let
        viewTurret { x, y, color } =
            circle color 25
                |> move x y
    in
    List.map viewTurret >> group


viewBullets : List Bullet -> Shape
viewBullets =
    let
        viewBullet { x, y } =
            circle black bulletRadius
                |> fade 0.8
                |> move x y
    in
    List.map viewBullet >> group


viewTimeBombs : List TimeBomb -> Shape
viewTimeBombs =
    let
        viewTimeBomb : TimeBomb -> Shape
        viewTimeBomb { x, y } =
            group
                [ circle red bulletRadius
                    |> fade 0.8
                , circle red timeBombBlastRadius
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
