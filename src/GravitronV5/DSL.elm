module GravitronV5.DSL exposing (main)

import Dict exposing (Dict)
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
    , vx : Number
    , vy : Number
    }


type SingletonDict
    = SingletonDict (Dict String Entity)


initSingletons : List EntityName -> SingletonDict
initSingletons names =
    List.map initialSingleton names
        |> List.foldl setSingleton (SingletonDict Dict.empty)


getSingleton : EntityName -> SingletonDict -> Entity
getSingleton name (SingletonDict dict) =
    case Dict.get (toString name) dict of
        Just entity ->
            entity

        Nothing ->
            initialSingleton name


mapSingletons : (Entity -> Entity) -> SingletonDict -> SingletonDict
mapSingletons func (SingletonDict dict) =
    Dict.map (\_ -> func) dict |> SingletonDict


setSingleton : Entity -> SingletonDict -> SingletonDict
setSingleton entity (SingletonDict dict) =
    SingletonDict (Dict.insert (toString entity.name) entity dict)


initialSingleton : EntityName -> Entity
initialSingleton =
    let
        i : EntityConfig -> Entity
        i { name, x, y, r, color, moveBehaviour } =
            { id = SingletonID name
            , name = name
            , x = x
            , y = y
            , r = r
            , color = color
            , moveBehaviour = moveBehaviour
            , vx = 0
            , vy = 0
            }
    in
    configOf >> i


type alias EntityConfig =
    { name : EntityName
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    , isSingleton : Bool
    , moveBehaviour : MoveBehaviour
    , bounceInScreen : Bool
    , health : Health
    , receivesDamageFrom : List EntityName
    , weaponConfig : WeaponConfig
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
                |> firesWeaponEvery 60
                |> hasHP
                |> receivesCollisionDamageFrom [ Bullet, BombBlast ]

        Bullet ->
            entityNamed Bullet
                |> hasGravitateToSingletonBehaviour Player
                |> hasBounceInScreenBehaviour 0.5
                |> isKilledOnCollisionWith [ Bullet, BombBlast, Turret ]

        TimeBomb ->
            entityNamed TimeBomb
                |> hasGravitateToSingletonBehaviour Player
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


type Health
    = Health


type WeaponConfig
    = NoWeapon


type DeathBehaviour
    = DeathBehaviour


entityNamed : EntityName -> EntityConfig
entityNamed name =
    { name = name
    , x = 0
    , y = 0
    , r = 10
    , color = blue
    , isSingleton = False
    , moveBehaviour = NoMovement
    , bounceInScreen = False
    , health = Health
    , receivesDamageFrom = []
    , weaponConfig = NoWeapon
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
firesWeaponEvery _ =
    identity


hasHP =
    identity


hasGravitateToSingletonBehaviour _ =
    identity


receivesCollisionDamageFrom _ =
    identity


isKilledOnCollisionWith _ =
    identity


onDeathSpawnsBombBlast =
    identity


hasBounceInScreenBehaviour _ =
    identity


isKilledOnNextUpdate =
    identity



-- Game


type Memory
    = Init
    | Memory Mem


type alias Mem =
    { singletons : SingletonDict
    , entityList : List Entity
    }


initialMemory : Mem
initialMemory =
    { singletons = initSingletons [ Player ]
    , entityList = []
    }


updateMemory : Computer -> Mem -> Mem
updateMemory computer mem =
    { mem | singletons = mapSingletons (updateEntity computer) mem.singletons }


updateEntity : Computer -> Entity -> Entity
updateEntity computer e =
    stepMovement computer e
        |> stepPosition


stepMovement : Computer -> Entity -> Entity
stepMovement { time } e =
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
            { e | x = zigzag -dx dx 1.92 time, y = wave -dy dy 2.11 time }


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
    List.map viewEntity (Dict.values sDict)


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
