module GravitronV5.DSL exposing (..)

import Dict exposing (Dict)
import Playground exposing (..)


type EntityName
    = Player
    | Turret
    | Bullet
    | TimeBomb
    | BombBlast


type alias Entity =
    {}


type SingletonDict
    = SingletonDict (Dict String Entity)


type alias EntityConfig =
    { name : EntityName
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
    = UUID Int


type MoveBehaviour
    = MoveBehaviour


type Health
    = Health


type WeaponConfig
    = NoWeapon
    | FiresEvery


type DeathBehaviour
    = DeathBehaviour


entityNamed =
    Debug.todo "impl"


singletonEntityNamed =
    Debug.todo "impl"


hasRandomWalkerBehaviour =
    Debug.todo "impl"


firesWeaponEvery =
    Debug.todo "impl"


hasHP =
    Debug.todo "impl"


hasGravitateToSingletonBehaviour =
    Debug.todo "impl"


receivesCollisionDamageFrom =
    Debug.todo "impl"


isKilledOnCollisionWith =
    Debug.todo "impl"


onDeathSpawnsBombBlast =
    Debug.todo "impl"


hasBounceInScreenBehaviour bounceFriction =
    Debug.todo "impl"


isKilledOnNextUpdate =
    Debug.todo "impl"
