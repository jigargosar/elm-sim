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


initialBulletRadius : Float
initialBulletRadius =
    6


initialTimeBombRadius : Float
initialTimeBombRadius =
    initialBulletRadius


initialTimeBombBlastRadius : Float
initialTimeBombBlastRadius =
    initialTimeBombRadius * 20


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
    , r : Number
    }


initPlayer : Number -> Number -> Player
initPlayer x y =
    let
        initialPlayerRadius =
            20
    in
    Player PlayerId x y initialPlayerRadius


type alias Turret =
    { id : Id
    , ct : Counter
    , x : Number
    , y : Number
    , color : Color
    , weapon : Weapon
    }


turretRadius : Number
turretRadius =
    25


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
    , r : Number
    , vx : Number
    , vy : Number
    }


initBullet : Id -> Number -> Number -> Number -> Number -> Number -> Bullet
initBullet id x y offset speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )

        ( dx, dy ) =
            fromPolar ( offset + initialBulletRadius + 1, angle )
    in
    Bullet id (x + dx) (y + dy) initialBulletRadius vx vy


type alias TimeBomb =
    { id : Id
    , x : Number
    , y : Number
    , r : Number
    , blastR : Number
    , vx : Number
    , vy : Number
    , ct : Counter
    }


initTimeBomb : Id -> Number -> Number -> Number -> Number -> Number -> TimeBomb
initTimeBomb id x y offset speed angle =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )

        ( dx, dy ) =
            fromPolar ( offset + initialTimeBombRadius + 1, angle )
    in
    TimeBomb id
        (x + dx)
        (y + dy)
        initialTimeBombRadius
        initialTimeBombBlastRadius
        vx
        vy
        (initCt (60 * 2))


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
playerToDamageCircle { id, x, y, r } =
    DamageCircle id x y r TagPlayer [ TagBullet, TagTimeBomb ]


blastToDamageCircle : Blast -> DamageCircle
blastToDamageCircle { id, x, y, r } =
    DamageCircle id x y r TagBlast [ TagTimeBomb, TagBullet ]


bulletToDamageCircle : Bullet -> DamageCircle
bulletToDamageCircle { id, x, y, r } =
    DamageCircle id x y r TagBullet [ TagTimeBomb, TagBullet ]


timeBombToDamageCircle : TimeBomb -> DamageCircle
timeBombToDamageCircle { id, x, y, r } =
    DamageCircle id x y r TagTimeBomb [ TagTimeBomb ]


type alias DamageCircle =
    { id : Id
    , x : Number
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
        && (target.id /= src.id)


type Res
    = AddExplosion Explosion
    | NewExplosion Number Number Number Color
    | AddBlast Blast
    | NewBlast Number Number Number
    | AddTurret Turret
    | AddBullet Bullet
    | NewBullet Number Number Number Number Number
    | AddTimeBomb TimeBomb
    | NewTimeBomb Number Number Number Number Number
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

                NewBullet x y r speed angle ->
                    reducer
                        (AddBullet (initBullet (BulletId nextId) x y r speed angle))
                        { mem | nextId = nextId + 1 }

                NewTimeBomb x y r speed angle ->
                    reducer
                        (AddTimeBomb
                            (initTimeBomb (TimeBombId nextId) x y r speed angle)
                        )
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
            , allDamageCircles =
                playerToDamageCircle player
                    :: List.map blastToDamageCircle blasts
                    ++ List.map timeBombToDamageCircle timeBombs
                    ++ List.map bulletToDamageCircle bullets
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


updatePlayer : Time -> Player -> Player
updatePlayer time p =
    { p | x = wave -100 100 11 time, y = wave -300 300 5 time }


stepTimeBombs :
    { a
        | scr : Screen
        , tx : Float
        , ty : Float
        , allDamageCircles : List DamageCircle
    }
    -> List TimeBomb
    -> List Res
stepTimeBombs { scr, tx, ty, allDamageCircles } =
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

        stepDead : TimeBomb -> Res
        stepDead { x, y, r, blastR } =
            Batch
                [ newExplosion x y r red
                , newBlast x y blastR
                ]

        step : TimeBomb -> Res
        step timeBomb =
            let
                dc =
                    List.filter
                        (isDamaging (timeBombToDamageCircle timeBomb))
                        allDamageCircles

                intersectionCount =
                    List.length dc
            in
            if
                isDone timeBomb.ct
                    || List.any
                        (isDamaging (timeBombToDamageCircle timeBomb))
                        allDamageCircles
            then
                stepDead timeBomb

            else
                stepAlive timeBomb
    in
    List.map step


stepBullets :
    { a
        | scr : Screen
        , tx : Float
        , ty : Float
        , allDamageCircles : List DamageCircle
    }
    -> List Bullet
    -> List Res
stepBullets { scr, tx, ty, allDamageCircles } =
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

        stepDead : Bullet -> Res
        stepDead { x, y, r } =
            newExplosion x y r black

        step : Bullet -> Res
        step bullet =
            if
                List.any
                    (isDamaging (bulletToDamageCircle bullet))
                    allDamageCircles
            then
                stepDead bullet

            else
                stepAlive bullet
    in
    List.map step


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
        fireWeaponOnCounter : Turret -> Res
        fireWeaponOnCounter t =
            [ if isDone t.ct then
                let
                    angle =
                        angleFromTo t.x t.y tx ty
                in
                case t.weapon of
                    BulletWeapon ->
                        NewBullet t.x t.y turretRadius 3 angle

                    TimeBombWeapon ->
                        NewTimeBomb t.x t.y turretRadius 3 angle

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
viewPlayer { x, y, r } =
    circle green r
        |> move x y


viewTurrets : List Turret -> Shape
viewTurrets =
    let
        viewTurret { x, y, color } =
            circle color turretRadius
                |> move x y
    in
    List.map viewTurret >> group


viewBullets : List Bullet -> Shape
viewBullets =
    let
        viewBullet { x, y, r } =
            circle black r
                |> fade 0.8
                |> move x y
    in
    List.map viewBullet >> group


viewTimeBombs : List TimeBomb -> Shape
viewTimeBombs =
    let
        viewTimeBomb : TimeBomb -> Shape
        viewTimeBomb { x, y, r, blastR } =
            group
                [ circle red r
                    |> fade 0.8
                , circle red blastR
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
