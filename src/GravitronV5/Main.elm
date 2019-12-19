module GravitronV5.Main exposing (main)

import Playground exposing (..)


type Id
    = Id ActorId Int


type ActorId
    = Player


type Behaviour
    = BounceOffScreen Number
    | RandomWalker
    | MoveByVelocity


type Point
    = Point Number Number


type alias Data =
    { pos : Point }


type Actor
    = Actor Id Data (List Behaviour)


type alias Mem =
    { actors : List Actor
    , nextId : Int
    }


initialPlayer id =
    Actor id
        (Data (Point 0 0))
        [ RandomWalker, BounceOffScreen 1, MoveByVelocity ]


initialMemory : Mem
initialMemory =
    { actors = [ initialPlayer (Id Player 0) ]
    , nextId = 100
    }


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    { mem | actors = List.map updateActor mem.actors }


updateActor : Actor -> Actor
updateActor =
    identity


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { actors } =
    List.map viewActor actors


viewActor : Actor -> Shape
viewActor (Actor (Id tag _) data _) =
    case tag of
        Player ->
            circle green 100
                |> move data.pos


move : Point -> Shape -> Shape
move (Point x y) =
    Playground.move x y


main =
    Playground.game viewMemory updateMemory initialMemory
