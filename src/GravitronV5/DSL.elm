module GravitronV5.DSL exposing (main)

import Basics.Extra exposing (flip, uncurry)
import Dict exposing (Dict)
import GravitronV5.Geom as Geom
import GravitronV5.HP as HP exposing (HP)
import Playground exposing (..)


type EntityName
    = Player
    | Turret
    | Bullet
    | TimeBomb
    | BombBlast


toString : EntityName -> String
toString name =
    case name of
        Player ->
            "PLAYER"

        Turret ->
            "TURRET"

        Bullet ->
            "BULLET"

        TimeBomb ->
            "TIME_BOMB"

        BombBlast ->
            "BOMB_BLAST"


type alias Entity =
    { id : UUID
    , name : EntityName
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    , moveBehaviour : MoveBehaviour
    , bounceInScreen : Maybe Number
    , weapon : Weapon
    , vx : Number
    , vy : Number
    , receivesDamageFrom : List EntityName
    , hp : HP
    }


setPos : Number -> Number -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
setPos x y e =
    { e | x = x, y = y }


movePos : Number -> Number -> { a | x : Number, y : Number } -> { a | x : Number, y : Number }
movePos dx dy ({ x, y } as e) =
    { e | x = x + dx, y = y + dy }


setVel : Number -> Number -> { c | vx : Number, vy : Number } -> { c | vx : Number, vy : Number }
setVel vx vy e =
    { e | vx = vx, vy = vy }


type SingletonDict
    = SingletonDict (Dict String Entity)


initSingletons : List EntityName -> SingletonDict
initSingletons names =
    List.map initSingleton names
        |> List.foldl setSingleton (SingletonDict Dict.empty)


getSingleton : EntityName -> SingletonDict -> Entity
getSingleton name (SingletonDict dict) =
    case Dict.get (toString name) dict of
        Just entity ->
            entity

        Nothing ->
            initSingleton name


singletonsValues : SingletonDict -> List Entity
singletonsValues (SingletonDict dict) =
    Dict.values dict


setSingleton : Entity -> SingletonDict -> SingletonDict
setSingleton entity (SingletonDict dict) =
    SingletonDict (Dict.insert (toString entity.name) entity dict)


entityFromIdConfig : UUID -> EntityConfig -> Entity
entityFromIdConfig id config =
    let
        { name, x, y, r, vx, vy, color } =
            config

        { moveBehaviour, bounceInScreen, weaponConfig, receivesDamageFrom, maxHP } =
            config
    in
    { id = id
    , name = name
    , x = x
    , y = y
    , r = r
    , color = color
    , moveBehaviour = moveBehaviour
    , bounceInScreen = bounceInScreen
    , vx = vx
    , vy = vy
    , weapon = Maybe.map weaponFromConfig weaponConfig |> Maybe.withDefault NoWeapon
    , receivesDamageFrom = receivesDamageFrom
    , hp = HP.withMax maxHP
    }


weaponFromConfig : WeaponConfig -> Weapon
weaponFromConfig =
    Weapon 0


initSingleton : EntityName -> Entity
initSingleton name =
    let
        config =
            configOf name
    in
    entityFromIdConfig (SingletonID name) config


initEntityWithIdName : UUID -> EntityName -> Entity
initEntityWithIdName id name =
    let
        config =
            configOf name
    in
    entityFromIdConfig id config


type alias EntityConfig =
    { name : EntityName
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    , isSingleton : Bool
    , moveBehaviour : MoveBehaviour
    , bounceInScreen : Maybe Number
    , maxHP : Int
    , receivesDamageFrom : List EntityName
    , weaponConfig : Maybe WeaponConfig
    , onDeath : DeathBehaviour
    }


configOf : EntityName -> EntityConfig
configOf name =
    case name of
        Player ->
            singletonEntityNamed Player
                |> withRadius 20
                |> withColor green
                |> hasRandomWalkerBehaviour
                |> hasBounceInScreenBehaviour 1

        Turret ->
            entityNamed Turret
                |> withRadius 25
                |> withColor red
                |> firesWeaponEvery 60
                |> withMaxHP 3
                |> receivesCollisionDamageFrom [ Bullet, BombBlast ]

        Bullet ->
            entityNamed Bullet
                |> withRadius 8
                |> withColor darkGray
                |> withGravitateToSingleton Player
                |> hasBounceInScreenBehaviour 0.5
                |> receivesCollisionDamageFrom [ BombBlast, Player, Turret, Bullet ]

        TimeBomb ->
            entityNamed TimeBomb
                |> withGravitateToSingleton Player
                |> receivesCollisionDamageFrom [ TimeBomb, BombBlast, Player, Turret, Bullet ]
                |> onDeathSpawnsBombBlast

        BombBlast ->
            entityNamed BombBlast
                |> isKilledOnNextUpdate


type UUID
    = SingletonID EntityName
    | UUID Int


type MoveBehaviour
    = NoMovement
    | RandomWalker
    | GravitateTo EntityName


type alias WeaponConfig =
    { every : Int, name : EntityName, towards : EntityName }


type Weapon
    = NoWeapon
    | Weapon Int WeaponConfig


type DeathBehaviour
    = DeathBehaviour


entityNamed : EntityName -> EntityConfig
entityNamed name =
    { name = name
    , x = 0
    , y = 0
    , r = 10
    , vx = 0
    , vy = 0
    , color = blue
    , isSingleton = False
    , moveBehaviour = NoMovement
    , bounceInScreen = Nothing
    , maxHP = 1
    , receivesDamageFrom = []
    , weaponConfig = Nothing
    , onDeath = DeathBehaviour
    }


singletonEntityNamed : EntityName -> EntityConfig
singletonEntityNamed =
    entityNamed >> (\c -> { c | isSingleton = True })


withRadius : Number -> EntityConfig -> EntityConfig
withRadius r c =
    { c | r = r }


withColor : Color -> EntityConfig -> EntityConfig
withColor color c =
    { c | color = color }


hasRandomWalkerBehaviour : EntityConfig -> EntityConfig
hasRandomWalkerBehaviour c =
    { c | moveBehaviour = RandomWalker }


firesWeaponEvery : Int -> EntityConfig -> EntityConfig
firesWeaponEvery every c =
    { c | weaponConfig = Just { every = every, name = Bullet, towards = Player } }


withMaxHP : Int -> EntityConfig -> EntityConfig
withMaxHP maxHP c =
    { c | maxHP = maxHP }


withGravitateToSingleton : EntityName -> EntityConfig -> EntityConfig
withGravitateToSingleton target c =
    { c | moveBehaviour = GravitateTo target }


receivesCollisionDamageFrom : List EntityName -> EntityConfig -> EntityConfig
receivesCollisionDamageFrom l c =
    { c | receivesDamageFrom = l }


onDeathSpawnsBombBlast =
    identity


hasBounceInScreenBehaviour : Number -> EntityConfig -> EntityConfig
hasBounceInScreenBehaviour bf e =
    { e | bounceInScreen = Just bf }


isKilledOnNextUpdate =
    identity



-- Game


type Memory
    = Init
    | Memory Mem


type alias Mem =
    { singletons : SingletonDict
    , entityList : List Entity
    , nextId : Int
    }


singletonNames =
    [ Player ]


initialMemory : Mem
initialMemory =
    let
        turrets =
            [ ( -150, 150 ) ]
                |> List.map
                    (\( x, y ) ->
                        initEntityWithIdName (UUID 1) Turret
                            |> setPos x y
                    )
    in
    { singletons = initSingletons singletonNames
    , entityList = turrets
    , nextId = 100
    }


type Response
    = NewEntity EntityConfig
    | UpdateEntity Entity
    | NoResponse
    | Batch (List Response)


updateMemory : Computer -> Mem -> Mem
updateMemory computer mem =
    let
        { singletons, entityList } =
            mem

        singletonList =
            singletonsValues singletons

        allEntities =
            singletonList ++ entityList

        stepBatch : List Entity -> Response
        stepBatch =
            List.map (stepEntity computer singletons allEntities) >> Batch
    in
    [ stepBatch singletonList
    , stepBatch entityList
    ]
        |> stepResponses mem


stepResponses : Mem -> List Response -> Mem
stepResponses =
    let
        stepOne : Response -> Mem -> Mem
        stepOne response mem =
            let
                { singletons, entityList, nextId } =
                    mem
            in
            case response of
                NoResponse ->
                    mem

                Batch responses ->
                    List.foldr stepOne mem responses

                NewEntity entityConfig ->
                    { mem
                        | entityList = entityFromIdConfig (UUID nextId) entityConfig :: entityList
                        , nextId = nextId + 1
                    }

                UpdateEntity entity ->
                    if List.member entity.name singletonNames then
                        { mem | singletons = setSingleton entity singletons }

                    else
                        { mem | entityList = entity :: entityList }
    in
    (\mem -> { mem | entityList = [] }) >> List.foldr stepOne


stepEntity : Computer -> SingletonDict -> List Entity -> Entity -> Response
stepEntity computer singletons others =
    let
        stepAlive e =
            updateMovement computer singletons e
                |> applyBounceInScreen computer.screen
                |> Geom.applyVel
                |> stepWeapon singletons

        preStep e =
            let
                hits =
                    List.filter (flip isCollidingWith e) others
                        |> List.length
            in
            { e | hp = HP.remove hits e.hp }

        step e =
            if HP.noneLeft e.hp then
                NoResponse

            else
                stepAlive e
    in
    preStep >> step


isCollidingWith : Entity -> Entity -> Bool
isCollidingWith other entity =
    List.member other.name entity.receivesDamageFrom
        && (entity.id /= other.id)
        && Geom.ccc entity.x entity.y entity.r other.x other.y other.r


stepWeapon : SingletonDict -> Entity -> Response
stepWeapon singletons e =
    case e.weapon of
        NoWeapon ->
            UpdateEntity e

        Weapon elapsed ({ every, name, towards } as weaponConfig) ->
            if elapsed >= every then
                let
                    player : Entity
                    player =
                        getSingleton Player singletons

                    projectileConfig : EntityConfig
                    projectileConfig =
                        configOf name

                    offset =
                        projectileConfig.r + e.r

                    angle =
                        atan2 (player.y - e.y) (player.x - e.x)

                    speed =
                        3

                    newProjectileConfig =
                        projectileConfig
                            |> setPos e.x e.y
                            |> uncurry movePos (fromPolar ( offset, angle ))
                            |> uncurry setVel (fromPolar ( speed, angle ))
                in
                Batch [ NewEntity newProjectileConfig, UpdateEntity { e | weapon = Weapon 0 weaponConfig } ]

            else
                UpdateEntity { e | weapon = Weapon (elapsed + 1) weaponConfig }


updateMovement : Computer -> SingletonDict -> Entity -> Entity
updateMovement { time, screen } singletons e =
    case e.moveBehaviour of
        NoMovement ->
            e

        RandomWalker ->
            setPos (wave screen.left screen.right 6 time) (wave screen.top screen.bottom 8 time) e

        GravitateTo targetName ->
            let
                { x, y } =
                    getSingleton targetName singletons
            in
            Geom.gravitateVelTo x y e


applyBounceInScreen : Screen -> Entity -> Entity
applyBounceInScreen screen e =
    case e.bounceInScreen of
        Just bounceFactor ->
            Geom.bounceVel bounceFactor screen e

        Nothing ->
            e



-- VIEW


viewMemory : Computer -> Mem -> List Shape
viewMemory _ m =
    let
        (SingletonDict sDict) =
            m.singletons
    in
    [ List.map viewEntity (Dict.values sDict)
    , List.map viewEntity m.entityList
    ]
        |> List.map group


viewEntity : Entity -> Shape
viewEntity { name, x, y, r, color, hp } =
    let
        moveToPosition =
            move x y

        hpShape =
            if List.member name [ Turret ] then
                words black (String.fromInt (HP.remaining hp))

            else
                group []

        toShape =
            group
                [ circle color r
                , hpShape
                ]
    in
    toShape |> moveToPosition


main =
    let
        view_ : Computer -> Memory -> List Shape
        view_ c m =
            case m of
                Init ->
                    []

                Memory mem ->
                    viewMemory c mem

        update_ : Computer -> Memory -> Memory
        update_ c m =
            case m of
                Init ->
                    Memory initialMemory

                Memory mem ->
                    Memory (updateMemory c mem)
    in
    game view_ update_ Init
