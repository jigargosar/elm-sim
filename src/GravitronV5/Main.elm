module GravitronV5.Main exposing (main)

import Playground exposing (..)


type Id
    = Id Int


type Tag
    = Player


type Data
    = Position Number Number
    | Radius Number
    | Velocity Number Number
    | PrimaryColour Color
    | PrimaryTag Tag
    | TriggerTimer Int Int


type Behaviour
    = BounceOffScreen Number
    | RandomWalker
    | ShootOnTriggerTimer
    | GravitateTo Tag
    | MoveByVelocity


type Actor
    = Actor Id (List Data) (List Behaviour)


type alias Mem =
    { actors : List Actor
    , nextId : Int
    }


initialPlayer id =
    Actor id
        [ Position 0 0, Radius 15, PrimaryColour green, PrimaryTag Player ]
        [ RandomWalker, BounceOffScreen 1, MoveByVelocity ]


initialMemory : Mem
initialMemory =
    { actors = [ initialPlayer (Id 0) ]
    , nextId = 100
    }


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    mem


viewMemory : Computer -> Mem -> List Shape
viewMemory _ _ =
    []


main =
    Playground.game viewMemory updateMemory initialMemory
