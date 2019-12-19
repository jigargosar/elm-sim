module GravitronV5.Main exposing (main)

import Playground exposing (..)


type Actor
    = Player Data


type alias Data =
    { kind : Tag
    , pos : Point
    }


type Tag
    = PlayerTag


type Point
    = Point Number Number


type alias Mem =
    { actors : List Actor
    , nextId : Int
    }


initialPlayer : Actor
initialPlayer =
    Player (Data PlayerTag (Point 0 0))


initialMemory : Mem
initialMemory =
    { actors = [ initialPlayer ]
    , nextId = 100
    }


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    { mem | actors = List.map updateActor mem.actors }


updateActor : Actor -> Actor
updateActor actor =
    case actor of
        Player ({ pos } as data) ->
            actor


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { actors } =
    List.map viewActor actors


viewActor : Actor -> Shape
viewActor actor =
    case actor of
        Player { pos } ->
            circle green 100
                |> move pos


move : Point -> Shape -> Shape
move (Point x y) =
    Playground.move x y


main =
    Playground.game viewMemory updateMemory initialMemory
