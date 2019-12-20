module GravitronV5.DSL exposing (..)

import Playground exposing (..)
import Random


type EntityType
    = Player
    | Turret
    | Bullet
    | TimeBomb


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


hasGravitateToBehaviour =
    Debug.todo "impl"


receivesCollisionDamageFrom =
    Debug.todo "impl"


isKilledBy =
    Debug.todo "impl"


hasBounceInScreenBehaviour bounceFriction =
    Debug.todo "impl"


entityList : List Entity
entityList =
    [ singletonEntityNamed Player
        |> hasRandomWalkerBehaviour
    , entityNamed Turret
        |> firesWeaponEvery 60
        |> hasHP
        |> receivesCollisionDamageFrom [ Bullet ]
    , entityNamed Bullet
        |> hasGravitateToBehaviour Player
        |> hasBounceInScreenBehaviour 0.5
        |> isKilledBy [ Bullet, Turret ]
    ]
