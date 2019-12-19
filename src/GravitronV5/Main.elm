module GravitronV5.Main exposing (main)

import Basics.Extra exposing (flip)
import GravitronV5.Body as TaggedCircle exposing (Body)
import GravitronV5.Counter as Counter exposing (Counter)
import GravitronV5.Geom as Geom
import GravitronV5.HP as HP exposing (HP)
import GravitronV5.Id as Id exposing (Id)
import GravitronV5.Player as Player exposing (Player)
import GravitronV5.Tag as Tag exposing (Tag)
import Playground exposing (..)
import PointFree exposing (anyPass, ifElse)



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
                Tag.TagTurret
                (x * factor)
                (y * factor)
                turretRadius
                color
                wep
                (Counter.initCt 160)
                (HP.fromMax maxHP)
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
    Bullet id Tag.TagBullet (x + dx) (y + dy) initialBulletRadius vx vy


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
        Tag.TagTimeBomb
        (x + dx)
        (y + dy)
        initialTimeBombRadius
        initialTimeBombBlastRadius
        vx
        vy
        (Counter.initCt (60 * 2))


type alias Blast =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    }


initBlast : Number -> Number -> Number -> Id -> Blast
initBlast x y r id =
    Blast id Tag.TagBlast x y r


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
    Explosion id (Counter.initCt 60) x y r c


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
    , player = Player.init 0 0
    , turrets =
        initTurrets
            [ TurretConfig (Id.Turret 0) red BulletWeapon 1
            , TurretConfig (Id.Turret 1) red BulletWeapon 1
            , TurretConfig (Id.Turret 2) blue TimeBombWeapon 2
            , TurretConfig (Id.Turret 3) orange BulletWeapon 3
            ]
    , bullets = []
    , timeBombs = []
    , blasts = []
    , explosions = []
    }



-- DamageModel


tagsWhichCanCauseDamageTo : Tag -> List Tag
tagsWhichCanCauseDamageTo targetTag =
    case targetTag of
        Tag.TagTimeBomb ->
            [ Tag.TagTimeBomb, Tag.TagBullet, Tag.TagBlast, Tag.TagPlayer, Tag.TagTurret ]

        Tag.TagBullet ->
            [ Tag.TagBullet, Tag.TagBlast, Tag.TagPlayer, Tag.TagTurret ]

        Tag.TagBlast ->
            []

        Tag.TagPlayer ->
            []

        Tag.TagTurret ->
            [ Tag.TagBullet, Tag.TagBlast ]


canCauseDamageTo : Body -> Body -> Bool
canCauseDamageTo target src =
    List.member src.tag (tagsWhichCanCauseDamageTo target.tag)
        && (src.id /= target.id)


isCausingDamageTo : Body -> Body -> Bool
isCausingDamageTo target src =
    let
        areIntersecting : Body -> Body -> Bool
        areIntersecting a b =
            Geom.ccc a.x a.y a.r b.x b.y b.r
    in
    canCauseDamageTo target src && areIntersecting target src


anyCausingDamageTo :
    { a | id : Id, tag : Tag, x : Number, y : Number, r : Number }
    -> List Body
    -> Bool
anyCausingDamageTo target =
    List.any (isCausingDamageTo (TaggedCircle.toBody target))


isDamagedByAnyOf :
    List Body
    -> { a | id : Id, tag : Tag, x : Number, y : Number, r : Number }
    -> Bool
isDamagedByAnyOf =
    flip anyCausingDamageTo


countHitsTo :
    { a | id : Id, tag : Tag, x : Number, y : Number, r : Number }
    -> List Body
    -> Int
countHitsTo target =
    List.filter (isCausingDamageTo (TaggedCircle.toBody target)) >> List.length



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
                    withNewId Id.Bullet
                        (initBullet x y r speed angle >> addBullet)

                NewTimeBomb x y offset speed angle ->
                    withNewId Id.TimeBomb
                        (initTimeBomb x y offset speed angle >> addTimeBomb)

                NewExplosion x y r c ->
                    withNewId Id.Explosion
                        (initExplosion x y r c >> addExplosion)

                NewBlast x y r ->
                    withNewId Id.Blast
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

        playerCommon =
            Player.toBody player

        env : { screen : Screen, tx : Number, ty : Number, entityList : List Body }
        env =
            { screen = screen
            , tx = playerCommon.x
            , ty = playerCommon.y
            , entityList =
                Player.toBody player
                    :: List.map TaggedCircle.toBody blasts
                    ++ List.map TaggedCircle.toBody timeBombs
                    ++ List.map TaggedCircle.toBody bullets
                    ++ List.map TaggedCircle.toBody turrets
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
        | player = Player.updatePlayer screen mouse time player
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
    if Counter.isDone e.ct then
        NoRes

    else
        AddExplosion { e | ct = Counter.stepCt e.ct }


stepTimeBomb :
    { a
        | screen : Screen
        , tx : Float
        , ty : Float
        , entityList : List Body
    }
    -> TimeBomb
    -> Res
stepTimeBomb { screen, tx, ty, entityList } =
    let
        tick : TimeBomb -> TimeBomb
        tick b =
            { b | bombTimer = Counter.stepCt b.bombTimer }

        aliveResponse : TimeBomb -> Res
        aliveResponse =
            Geom.gravitateVelTo tx ty
                >> Geom.bounceVel 0.5 screen
                >> Geom.addVelToPos
                >> tick
                >> AddTimeBomb

        deathResponse : TimeBomb -> Res
        deathResponse { x, y, r, blastR } =
            Batch
                [ NewExplosion x y r red
                , NewBlast x y blastR
                ]
    in
    ifElse (anyPass [ .bombTimer >> Counter.isDone, isDamagedByAnyOf entityList ])
        deathResponse
        aliveResponse


stepBullet :
    { a
        | screen : Screen
        , tx : Float
        , ty : Float
        , entityList : List Body
    }
    -> Bullet
    -> Res
stepBullet { screen, tx, ty, entityList } =
    let
        aliveResponse : Bullet -> Res
        aliveResponse =
            Geom.gravitateVelTo tx ty
                >> Geom.bounceVel 0.5 screen
                >> Geom.addVelToPos
                >> AddBullet

        deathResponse : Bullet -> Res
        deathResponse { x, y, r } =
            NewExplosion x y r black
    in
    ifElse (isDamagedByAnyOf entityList)
        deathResponse
        aliveResponse


stepTurret :
    { a
        | tx : Float
        , ty : Float
        , entityList : List Body
    }
    -> Turret
    -> Res
stepTurret { tx, ty, entityList } =
    let
        fireWeaponResponse : Turret -> Res
        fireWeaponResponse { x, y, r, weapon } =
            let
                angle =
                    Geom.angleFromTo x y tx ty

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
            [ if Counter.isDone triggerCt then
                fireWeaponResponse t

              else
                NoRes
            , AddTurret { t | triggerCt = Counter.stepCt triggerCt }
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
            { t | hp = HP.remove hits t.hp }
    in
    stepHP
        >> ifElse (.hp >> HP.noneLeft)
            deathResponse
            aliveResponse



-- View


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { player, turrets, bullets, timeBombs, explosions } =
    [ Player.view player
    , viewTurrets turrets
    , viewBullets bullets
    , viewTimeBombs timeBombs
    , viewExplosions explosions
    ]


viewTurrets : List Turret -> Shape
viewTurrets =
    let
        viewTurret { x, y, r, color, hp } =
            group
                [ circle color r
                , words black (String.fromInt (HP.remaining hp))
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
                    Counter.ctProgress ct
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
