module GravitronV6.Entity exposing (..)

import GravitronV6.Circ as Circ
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)


type New
    = New Entity


type alias NewEntity =
    New


new : Entity -> NewEntity
new =
    New


findNamed : String -> List Entity -> Maybe Entity
findNamed name =
    List.Extra.find (propEq .name name)


type alias FireModel =
    { elapsed : Number
    , didTrigger : Bool
    , every : Number
    , towards : String
    , speed : Float
    , template : Entity
    }


type AliveStep
    = WalkRandomly
    | GravitateTo String
    | Wanderer
    | BounceInScreen Number
    | Fire FireModel
    | DieOnCollisionWith (List String)
    | ReceiveCollisionDamageFrom (List String)


wander : Computer -> Entity -> Entity
wander { screen } e =
    e


performRandomWalk : Computer -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
performRandomWalk { time, screen } e =
    let
        ( x, y ) =
            ( wave screen.left screen.right 6 time
            , wave screen.top screen.bottom 8 time
            )
    in
    { e | x = x, y = y }


performFire : Entity -> List Entity -> FireModel -> List Entity
performFire from allEntities fireModel =
    if fireModel.didTrigger then
        case findNamed fireModel.towards allEntities of
            Just to ->
                [ Circ.shoot from to fireModel.speed fireModel.template ]

            Nothing ->
                []

    else
        []


gravitateTo : List Entity -> String -> Entity -> Entity
gravitateTo allEntities towardsName entity =
    case findNamed towardsName allEntities of
        Just to ->
            Circ.gravitateTo to entity

        Nothing ->
            entity


type Phase
    = SpawningPhase SpawningModel
    | ReadyPhase
    | DyingPhase DyingModel


type alias SpawningModel =
    { elapsed : Number, duration : Number }


type alias DyingModel =
    { elapsed : Number, duration : Number }


type alias Entity =
    { id : Number
    , name : String
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    , vx : Number
    , vy : Number
    , maxHP : Number
    , currentHP : Number
    , aliveSteps : List AliveStep
    , phase : Phase
    }


kill : Entity -> Entity
kill entity =
    { entity | currentHP = 0 }


takeDamage : Number -> Entity -> Entity
takeDamage hits entity =
    let
        newHP =
            entity.currentHP
                - hits
                |> max 0
    in
    if newHP <= 0 then
        kill entity

    else
        { entity | currentHP = newHP }


invalidId =
    -1


default : Entity
default =
    { id = invalidId
    , name = "Unknown"
    , x = 0
    , y = 0
    , r = 100
    , color = blue
    , vx = 0
    , vy = 0
    , maxHP = 1
    , currentHP = 1
    , aliveSteps = []
    , phase = ReadyPhase
    }


moveByVelocity : { a | x : Number, vx : Number, y : Number, vy : Number } -> { a | x : Number, vx : Number, y : Number, vy : Number }
moveByVelocity e =
    { e | x = e.x + e.vx, y = e.y + e.vy }


withAliveSteps : a -> { b | aliveSteps : a } -> { b | aliveSteps : a }
withAliveSteps aliveSteps e =
    { e | aliveSteps = aliveSteps }


withHP : Number -> Entity -> Entity
withHP maxHp entity =
    { entity | maxHP = maxHp, currentHP = maxHp }


withColor : Color -> Entity -> Entity
withColor color entity =
    { entity | color = color }
