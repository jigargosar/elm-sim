module GravitronV5.Main exposing (main)

import Playground exposing (..)



-- Geom


type Point
    = Point Number Number


type Velocity
    = Velocity Number Number



-- Actor


type Actor
    = Player Data


type alias Data =
    { tag : Tag
    , pos : Point
    , vel : Velocity
    }


type Tag
    = PlayerTag



-- Player


initialPlayer : Actor
initialPlayer =
    Player (Data PlayerTag (Point 0 0) (Velocity 0 0))



-- Memory


type alias Mem =
    { actors : List Actor
    , nextId : Int
    }


initialMemory : Mem
initialMemory =
    { actors = [ initialPlayer ]
    , nextId = 100
    }



-- Update


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    { mem | actors = List.map updateActor mem.actors }


updateActor : Actor -> Actor
updateActor actor =
    case actor of
        Player ({ pos } as data) ->
            actor



-- View


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



-- Main


main =
    Playground.game viewMemory updateMemory initialMemory
