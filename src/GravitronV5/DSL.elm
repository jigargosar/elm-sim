module GravitronV5.DSL exposing (main)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import GravitronV5.Geom as Geom
import List.Extra
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
    , weapon : Weapon
    , vx : Number
    , vy : Number
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


singletonsFromList : List Entity -> SingletonDict
singletonsFromList =
    List.foldl setSingleton (SingletonDict Dict.empty)


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
entityFromIdConfig id { name, x, y, r, vx, vy, color, moveBehaviour, weaponConfig } =
    { id = id
    , name = name
    , x = x
    , y = y
    , r = r
    , color = color
    , moveBehaviour = moveBehaviour
    , vx = vx
    , vy = vy
    , weapon = Maybe.map weaponFromConfig weaponConfig |> Maybe.withDefault NoWeapon
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
    , health : Health
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
                |> hasHP
                |> receivesCollisionDamageFrom [ Bullet, BombBlast ]

        Bullet ->
            entityNamed Bullet
                |> withRadius 8
                |> withColor darkGray
                |> withGravitateToSingleton Player
                |> hasBounceInScreenBehaviour 0.5
                |> isKilledOnCollisionWith [ Bullet, BombBlast, Turret ]

        TimeBomb ->
            entityNamed TimeBomb
                |> withGravitateToSingleton Player
                |> isKilledOnCollisionWith [ Bullet, BombBlast, Turret, TimeBomb ]
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


type Health
    = Health


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
    , health = Health
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


hasHP =
    identity


withGravitateToSingleton : EntityName -> EntityConfig -> EntityConfig
withGravitateToSingleton target c =
    { c | moveBehaviour = GravitateTo target }


receivesCollisionDamageFrom _ =
    identity


isKilledOnCollisionWith _ =
    identity


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
    { singletons = initSingletons [ Player ]
    , entityList = turrets
    , nextId = 100
    }


updateMemory : Computer -> Mem -> Mem
updateMemory computer mem =
    let
        { singletons, entityList } =
            mem

        updateHelp : Entity -> ( List EntityConfig, Entity )
        updateHelp =
            updateEntity computer singletons

        ( newConfigs1, newSingletonList ) =
            List.Extra.mapAccuml
                (\acc e ->
                    updateHelp e
                        |> Tuple.mapFirst ((++) acc)
                )
                []
                (singletonsValues singletons)

        ( newConfigs2, newEntityList ) =
            List.Extra.mapAccuml
                (\acc e ->
                    updateHelp e
                        |> Tuple.mapFirst ((++) acc)
                )
                []
                entityList

        newConfigs : List EntityConfig
        newConfigs =
            [ newConfigs1, newConfigs2 ] |> List.concat
    in
    { mem
        | singletons = singletonsFromList newSingletonList
        , entityList = newEntityList
    }
        |> addNewFromConfigs newConfigs


addNewFromConfigs : List EntityConfig -> Mem -> Mem
addNewFromConfigs =
    let
        addNew c mem =
            { mem | entityList = entityFromIdConfig (UUID mem.nextId) c :: mem.entityList }
    in
    \newConfigs mem -> List.foldl addNew mem newConfigs


updateEntity : Computer -> SingletonDict -> Entity -> ( List EntityConfig, Entity )
updateEntity computer singletons e =
    stepMovement computer singletons e
        |> stepPosition
        |> stepWeapon singletons


stepWeapon : SingletonDict -> Entity -> ( List EntityConfig, Entity )
stepWeapon singletons e =
    case e.weapon of
        NoWeapon ->
            ( [], e )

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
                ( [ newProjectileConfig ], { e | weapon = Weapon 0 weaponConfig } )

            else
                ( [], { e | weapon = Weapon (elapsed + 1) weaponConfig } )


stepMovement : Computer -> SingletonDict -> Entity -> Entity
stepMovement { time } singletons e =
    case e.moveBehaviour of
        NoMovement ->
            e

        RandomWalker ->
            let
                dx =
                    220

                dy =
                    400
            in
            setPos (zigzag -dx dx 1.92 time) (wave -dy dy 2.11 time) e

        GravitateTo targetName ->
            let
                player : Entity
                player =
                    getSingleton targetName singletons
            in
            Geom.gravitateVelTo player.x player.y e


stepPosition : Entity -> Entity
stepPosition e =
    let
        { x, y, vx, vy } =
            e
    in
    { e | x = x + vx, y = y + vy }


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
viewEntity { x, y, r, color } =
    circle color r
        |> move x y


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
