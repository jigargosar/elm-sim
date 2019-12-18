module GravitronV4.Main exposing (main)

import Basics.Extra exposing (flip)
import Playground exposing (..)
import PointFree exposing (anyPass, ifElse)



-- Counter


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



-- HP


type HP
    = HP Int Int


initHP : Int -> HP
initHP mx =
    HP (max 0 mx) (max 0 mx)


decHPBy : Int -> HP -> HP
decHPBy hits (HP mx n) =
    HP mx (clamp 0 mx (n - hits))


remainingHP : HP -> Int
remainingHP (HP _ n) =
    n


noHPLeft : HP -> Bool
noHPLeft (HP _ n) =
    n <= 0



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
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    }


initPlayer : Number -> Number -> Player
initPlayer x y =
    let
        initialPlayerRadius =
            20
    in
    Player PlayerId TagPlayer x y initialPlayerRadius 0 0


type alias Turret =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    , weapon : Weapon
    , triggerCt : Counter
    , hp : HP
    }


type Weapon
    = BulletWeapon
    | TimeBombWeapon


type TurretConfig
    = TurretConfig Id Color Weapon Int


initTurrets : List TurretConfig -> List Turret
initTurrets =
    let
        positions =
            [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ]

        factor =
            150

        turretRadius : Number
        turretRadius =
            25

        initTurret ( x, y ) (TurretConfig id color wep maxHP) =
            Turret id
                TagTurret
                (x * factor)
                (y * factor)
                turretRadius
                color
                wep
                (initCt 160)
                (initHP maxHP)
    in
    List.map2 initTurret positions


type alias Bullet =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    }


initBullet : Number -> Number -> Number -> Number -> Number -> Id -> Bullet
initBullet x y offset speed angle id =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )

        ( dx, dy ) =
            fromPolar ( offset + initialBulletRadius + 1, angle )
    in
    Bullet id TagBullet (x + dx) (y + dy) initialBulletRadius vx vy


type alias TimeBomb =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , blastR : Number
    , vx : Number
    , vy : Number
    , bombTimer : Counter
    }


initTimeBomb : Number -> Number -> Number -> Number -> Number -> Id -> TimeBomb
initTimeBomb x y offset speed angle id =
    let
        ( vx, vy ) =
            fromPolar ( speed, angle )

        ( dx, dy ) =
            fromPolar ( offset + initialTimeBombRadius + 1, angle )
    in
    TimeBomb id
        TagTimeBomb
        (x + dx)
        (y + dy)
        initialTimeBombRadius
        initialTimeBombBlastRadius
        vx
        vy
        (initCt (60 * 2))


type alias Blast =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    }


initBlast : Number -> Number -> Number -> Id -> Blast
initBlast x y r id =
    Blast id TagBlast x y r


type alias Explosion =
    { id : Id
    , ct : Counter
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    }


initExplosion : Number -> Number -> Number -> Color -> Id -> Explosion
initExplosion x y r c id =
    Explosion id (initCt 60) x y r c


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
            [ TurretConfig (TurretId 0) red BulletWeapon 1
            , TurretConfig (TurretId 1) red BulletWeapon 1
            , TurretConfig (TurretId 2) blue TimeBombWeapon 2
            , TurretConfig (TurretId 3) orange BulletWeapon 3
            ]
    , bullets = []
    , timeBombs = []
    , blasts = []
    , explosions = []
    }



-- Tags


type Tag
    = TagTimeBomb
    | TagBullet
    | TagBlast
    | TagPlayer
    | TagTurret


tagsWhichCanCauseDamageTo : Tag -> List Tag
tagsWhichCanCauseDamageTo targetTag =
    case targetTag of
        TagTimeBomb ->
            [ TagTimeBomb, TagBullet, TagBlast, TagPlayer, TagTurret ]

        TagBullet ->
            [ TagBullet, TagBlast, TagPlayer, TagTurret ]

        TagBlast ->
            []

        TagPlayer ->
            []

        TagTurret ->
            [ TagBullet, TagBlast ]



-- Entity with common props


type Entity
    = Entity EntityRec


type alias EntityRec =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    }


toTaggedCircle : { a | id : Id, tag : Tag, x : Number, y : Number, r : Number } -> Entity
toTaggedCircle { id, tag, x, y, r } =
    EntityRec id tag x y r
        |> Entity


canCauseDamageTo : EntityRec -> EntityRec -> Bool
canCauseDamageTo target src =
    List.member src.tag (tagsWhichCanCauseDamageTo target.tag)
        && (src.id /= target.id)


isCausingDamageTo : Entity -> Entity -> Bool
isCausingDamageTo (Entity target) (Entity src) =
    let
        areIntersecting : EntityRec -> EntityRec -> Bool
        areIntersecting a b =
            ccc a.x a.y a.r b.x b.y b.r
    in
    canCauseDamageTo target src && areIntersecting target src


anyCausingDamageTo :
    { a | id : Id, tag : Tag, x : Number, y : Number, r : Number }
    -> List Entity
    -> Bool
anyCausingDamageTo target =
    List.any (isCausingDamageTo (toTaggedCircle target))


isDamagedByAnyOf :
    List Entity
    -> { a | id : Id, tag : Tag, x : Number, y : Number, r : Number }
    -> Bool
isDamagedByAnyOf =
    flip anyCausingDamageTo


countHitsTo :
    { a | id : Id, tag : Tag, x : Number, y : Number, r : Number }
    -> List Entity
    -> Int
countHitsTo target =
    List.filter (isCausingDamageTo (toTaggedCircle target)) >> List.length



--  Update


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


withNewId : (Int -> Id) -> (Id -> Mem -> Mem) -> Mem -> Mem
withNewId toId func mem =
    func (toId mem.nextId) { mem | nextId = mem.nextId + 1 }


emptyListsThenProcessResponses : List Res -> Mem -> Mem
emptyListsThenProcessResponses =
    let
        addExplosion : Explosion -> Mem -> Mem
        addExplosion explosion mem =
            { mem | explosions = explosion :: mem.explosions }

        addBlast : Blast -> Mem -> Mem
        addBlast blast mem =
            { mem | blasts = blast :: mem.blasts }

        addTurret : Turret -> Mem -> Mem
        addTurret turret mem =
            { mem | turrets = turret :: mem.turrets }

        addBullet : Bullet -> Mem -> Mem
        addBullet bullet mem =
            { mem | bullets = bullet :: mem.bullets }

        addTimeBomb : TimeBomb -> Mem -> Mem
        addTimeBomb timeBomb mem =
            { mem | timeBombs = timeBomb :: mem.timeBombs }

        reducer : Res -> Mem -> Mem
        reducer res =
            case res of
                AddExplosion explosion ->
                    addExplosion explosion

                AddBlast blast ->
                    addBlast blast

                AddTurret turret ->
                    addTurret turret

                AddBullet bullet ->
                    addBullet bullet

                AddTimeBomb timeBomb ->
                    addTimeBomb timeBomb

                NewBullet x y r speed angle ->
                    withNewId BulletId
                        (initBullet x y r speed angle >> addBullet)

                NewTimeBomb x y offset speed angle ->
                    withNewId TimeBombId
                        (initTimeBomb x y offset speed angle >> addTimeBomb)

                NewExplosion x y r c ->
                    withNewId ExplosionId
                        (initExplosion x y r c >> addExplosion)

                NewBlast x y r ->
                    withNewId BlastId
                        (initBlast x y r >> addBlast)

                NoRes ->
                    identity

                Batch lst ->
                    foldHelp lst

        foldHelp : List Res -> Mem -> Mem
        foldHelp resList mem =
            List.foldl reducer mem resList

        reverseLists : Mem -> Mem
        reverseLists mem =
            { mem
                | turrets = List.reverse mem.turrets
                , bullets = List.reverse mem.bullets
                , timeBombs = List.reverse mem.timeBombs
                , blasts = List.reverse mem.blasts
                , explosions = List.reverse mem.explosions
            }

        emptyLists : Mem -> Mem
        emptyLists mem =
            { mem
                | turrets = []
                , bullets = []
                , timeBombs = []
                , blasts = []
                , explosions = []
            }
    in
    \resList -> emptyLists >> foldHelp resList >> reverseLists


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    let
        { turrets, player, explosions, blasts, bullets, timeBombs } =
            mem

        env : { screen : Screen, tx : Number, ty : Number, entityList : List Entity }
        env =
            { screen = screen
            , tx = player.x
            , ty = player.y
            , entityList =
                toTaggedCircle player
                    :: List.map toTaggedCircle blasts
                    ++ List.map toTaggedCircle timeBombs
                    ++ List.map toTaggedCircle bullets
                    ++ List.map toTaggedCircle turrets
            }

        mapBatch : (a -> Res) -> List a -> Res
        mapBatch func lst =
            Batch (List.map func lst)

        allResponses : List Res
        allResponses =
            [ mapBatch stepBlast blasts
            , mapBatch stepExplosion explosions
            , mapBatch (stepTurret env) turrets
            , mapBatch (stepBullet env) bullets
            , mapBatch (stepTimeBomb env) timeBombs
            ]
    in
    { mem
        | player = updatePlayer screen mouse time player
    }
        |> emptyListsThenProcessResponses allResponses
        |> nextLevel


nextLevel : Mem -> Mem
nextLevel mem =
    if List.isEmpty mem.turrets then
        { mem | turrets = initialMemory.turrets }

    else
        mem


stepBlast : Blast -> Res
stepBlast { x, y, r } =
    NewExplosion x y r red


stepExplosion : Explosion -> Res
stepExplosion e =
    if isDone e.ct then
        NoRes

    else
        AddExplosion { e | ct = stepCt e.ct }


updatePlayer : Screen -> Mouse -> Time -> Player -> Player
updatePlayer screen mouse time =
    let
        randomVel : Player -> Player
        randomVel ({ vx, vy } as p) =
            let
                ( dx, dy ) =
                    ( wave -100 100 11 time, wave -300 300 21 time )
                        |> toPolar
                        |> Tuple.mapFirst ((*) 0.0005)
                        |> fromPolar
            in
            { p | vx = vx + dx, vy = vy + dy }
    in
    if mouse.down then
        springToMouse mouse >> addVelToPos

    else
        randomVel >> bounceVel 1 screen >> addVelToPos


stepTimeBomb :
    { a
        | screen : Screen
        , tx : Float
        , ty : Float
        , entityList : List Entity
    }
    -> TimeBomb
    -> Res
stepTimeBomb { screen, tx, ty, entityList } =
    let
        tick : TimeBomb -> TimeBomb
        tick b =
            { b | bombTimer = stepCt b.bombTimer }

        aliveResponse : TimeBomb -> Res
        aliveResponse =
            gravitateVelTo tx ty
                >> bounceVel 0.5 screen
                >> addVelToPos
                >> tick
                >> AddTimeBomb

        deathResponse : TimeBomb -> Res
        deathResponse { x, y, r, blastR } =
            Batch
                [ NewExplosion x y r red
                , NewBlast x y blastR
                ]
    in
    ifElse (anyPass [ .bombTimer >> isDone, isDamagedByAnyOf entityList ])
        deathResponse
        aliveResponse


stepBullet :
    { a
        | screen : Screen
        , tx : Float
        , ty : Float
        , entityList : List Entity
    }
    -> Bullet
    -> Res
stepBullet { screen, tx, ty, entityList } =
    let
        stepAlive : Bullet -> Res
        stepAlive =
            gravitateVelTo tx ty
                >> bounceVel 0.5 screen
                >> addVelToPos
                >> AddBullet

        stepDead : Bullet -> Res
        stepDead { x, y, r } =
            NewExplosion x y r black
    in
    ifElse (isDamagedByAnyOf entityList)
        stepDead
        stepAlive


stepTurret :
    { a
        | tx : Float
        , ty : Float
        , entityList : List Entity
    }
    -> Turret
    -> Res
stepTurret { tx, ty, entityList } =
    let
        fireWeapon : Turret -> Res
        fireWeapon { x, y, r, weapon } =
            let
                angle =
                    angleFromTo x y tx ty

                speed =
                    3
            in
            case weapon of
                BulletWeapon ->
                    NewBullet x y r speed angle

                TimeBombWeapon ->
                    NewTimeBomb x y r speed angle

        aliveResponse : Turret -> Res
        aliveResponse ({ x, y, r, weapon, triggerCt, hp } as t) =
            [ if isDone triggerCt then
                fireWeapon t

              else
                NoRes
            , AddTurret { t | triggerCt = stepCt triggerCt }
            ]
                |> Batch

        deathResponse : Turret -> Res
        deathResponse { x, y, r, color } =
            NewExplosion x y r color

        stepHP : Turret -> Turret
        stepHP t =
            let
                hits =
                    countHitsTo t entityList
            in
            { t | hp = decHPBy hits t.hp }
    in
    stepHP
        >> ifElse (.hp >> noHPLeft)
            deathResponse
            aliveResponse



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
        viewTurret { x, y, r, color, hp } =
            group
                [ circle color r
                , words black (String.fromInt (remainingHP hp))
                ]
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


springToMouse : { a | x : Float, y : Float } -> { b | x : Float, y : Float, vx : Float, vy : Float } -> { b | x : Float, y : Float, vx : Float, vy : Float }
springToMouse mouse ({ x, y, vx, vy } as p) =
    let
        k =
            0.2

        f =
            0.5

        ( dx, dy ) =
            ( (mouse.x - x) * k, (mouse.y - y) * k )
    in
    { p | vx = (vx + dx) * f, vy = (vy + dy) * f }


addVelToPos : { a | x : number, vx : number, y : number, vy : number } -> { a | x : number, vx : number, y : number, vy : number }
addVelToPos b =
    { b | x = b.x + b.vx, y = b.y + b.vy }


gravitateVelTo :
    Float
    -> Float
    -> { a | x : Float, y : Float, vx : Float, vy : Float }
    -> { a | x : Float, y : Float, vx : Float, vy : Float }
gravitateVelTo tx ty b =
    let
        ( dx, dy ) =
            ( tx - b.x, ty - b.y )
                |> toPolar
                |> Tuple.mapFirst (\m -> 20 / m)
                |> fromPolar
    in
    { b | vx = b.vx + dx, vy = b.vy + dy }


bounceVel : Float -> Screen -> { a | x : Float, y : Float, vx : Float, vy : Float } -> { a | x : Float, y : Float, vx : Float, vy : Float }
bounceVel bounceFactor screen ({ x, y, vx, vy } as b) =
    let
        ( nvx, nvy ) =
            newBounceVelInScreen bounceFactor screen x y vx vy
    in
    { b | vx = nvx, vy = nvy }


newBounceVelInScreen :
    Float
    -> Screen
    -> Float
    -> Float
    -> Float
    -> Float
    -> ( Float, Float )
newBounceVelInScreen bounceFriction scr x y vx vy =
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
