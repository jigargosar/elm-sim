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
    | FiresEvery


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


type alias Mem =
    { singletons : SingletonDict }


initialMemory : Mem
initialMemory =
    { singletons =
        SingletonDict Dict.empty
            |> setSingleton (initialSingleton Player)
    }


updateMemory : Computer -> Mem -> Mem
updateMemory _ =
    identity


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
    game viewMemory updateMemory initialMemory
