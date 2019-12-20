module GravitronV5.DSL exposing (..)

import Playground exposing (..)
import Random


type EntityType
    = Player
    | Turret
    | Bullet
    | TimeBomb
    | BombBlast


type UUID
    = UUID Int


type Entity
    = Entity


type alias CommonProps =
    { uuid : UUID
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    }


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


entityList : List Entity
entityList =
    [ singletonEntityNamed Player
        |> hasRandomWalkerBehaviour
    , entityNamed Turret
        |> firesWeaponEvery 60
        |> hasHP
        |> receivesCollisionDamageFrom [ Bullet, BombBlast ]
    , entityNamed Bullet
        |> hasGravitateToSingletonBehaviour Player
        |> hasBounceInScreenBehaviour 0.5
        |> isKilledOnCollisionWith [ Bullet, BombBlast, Turret ]
    , entityNamed TimeBomb
        |> hasGravitateToSingletonBehaviour Player
        |> isKilledOnCollisionWith [ Bullet, BombBlast, Turret, TimeBomb ]
        |> onDeathSpawnsBombBlast
    , entityNamed BombBlast
        |> isKilledOnNextUpdate
    ]
