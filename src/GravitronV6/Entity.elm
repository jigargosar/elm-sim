module GravitronV6.Entity exposing (..)

import GravitronV6.Circ as Circ
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)


type PreStep
    = PreStep


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
    | Fire FireModel


findNamed : a -> List { b | name : a } -> Maybe { b | name : a }
findNamed name =
    List.Extra.find (propEq .name name)


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


updateAliveSteps : Entity -> Entity
updateAliveSteps entity =
    let
        func : AliveStep -> AliveStep
        func aliveStep =
            case aliveStep of
                WalkRandomly ->
                    aliveStep

                Fire rec ->
                    let
                        didTrigger =
                            rec.elapsed >= rec.every

                        newRec =
                            if didTrigger then
                                { rec | elapsed = 0 }

                            else
                                { rec | elapsed = rec.elapsed + 1 }
                    in
                    Fire { newRec | didTrigger = didTrigger }
    in
    withAliveSteps (List.map func entity.aliveSteps) entity


type DeathStep
    = DeathStep


type Phase
    = Spawning
    | Alive
    | Dying


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
    , preSteps : List PreStep
    , aliveSteps : List AliveStep
    , deathSteps : List DeathStep
    , phase : Phase
    }


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
    , preSteps = []
    , aliveSteps = []
    , deathSteps = []
    , phase = Alive
    }


performRandomWalk : Computer -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
performRandomWalk { time, screen } e =
    let
        ( x, y ) =
            ( wave screen.left screen.right 6 time
            , wave screen.top screen.bottom 8 time
            )
    in
    { e | x = x, y = y }


moveByVelocity : { a | x : Number, vx : Number, y : Number, vy : Number } -> { a | x : Number, vx : Number, y : Number, vy : Number }
moveByVelocity e =
    { e | x = e.x + e.vx, y = e.y + e.vy }


withAliveSteps : a -> { b | aliveSteps : a } -> { b | aliveSteps : a }
withAliveSteps aliveSteps e =
    { e | aliveSteps = aliveSteps }
