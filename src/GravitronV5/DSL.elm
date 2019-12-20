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
    , moveBehaviour : MoveBehaviour
    }


type SingletonDict
    = SingletonDict (Dict String Entity)


getSingleton : EntityName -> SingletonDict -> Entity
getSingleton name (SingletonDict dict) =
    case Dict.get (toString name) dict of
        Just entity ->
            entity

        Nothing ->
            initialSingleton name


initialSingleton : EntityName -> Entity
initialSingleton =
    let
        i : EntityConfig -> Entity
        i { name, x, y, r, moveBehaviour } =
            { id = SingletonID name
            , name = name
            , x = x
            , y = y
            , r = r
            , moveBehaviour = moveBehaviour
            }
    in
    configOf >> i


type alias EntityConfig =
    { name : EntityName
    , x : Number
    , y : Number
    , r : Number
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
    = MoveBehaviour


type Health
    = Health


type WeaponConfig
    = NoWeapon
    | FiresEvery


type DeathBehaviour
    = DeathBehaviour


entityNamed : EntityName -> EntityConfig
entityNamed name =
    { name = name
    , x = 0
    , y = 0
    , r = 10
    , isSingleton = False
    , moveBehaviour = MoveBehaviour
    , bounceInScreen = False
    , health = Health
    , receivesDamageFrom = []
    , weaponConfig = NoWeapon
    , onDeath = DeathBehaviour
    }


singletonEntityNamed : EntityName -> EntityConfig
singletonEntityNamed =
    entityNamed >> (\c -> { c | isSingleton = True })


hasRandomWalkerBehaviour : EntityConfig -> EntityConfig
hasRandomWalkerBehaviour =
    identity


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


type alias Mem =
    { singletons : SingletonDict }


initialMemory : Mem
initialMemory =
    { singletons = SingletonDict Dict.empty }


updateMemory : Computer -> Mem -> Mem
updateMemory _ =
    identity


viewMemory _ _ =
    []


main =
    game viewMemory updateMemory initialMemory
