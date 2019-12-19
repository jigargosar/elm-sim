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
updateMemory computer mem =
    { mem | actors = List.map (updateActor computer) mem.actors }


updateActor : Computer -> Actor -> Actor
updateActor { time } actor =
    case actor of
        Player data ->
            updatePlayer time data
                |> Player


updatePlayer : Time -> Data -> Data
updatePlayer time data =
    let
        updateVel d =
            let
                (Velocity vx vy) =
                    d.vel
            in
            { d | vel = Velocity (wave -2 2 3 time) (wave -1 1 5 time) }

        moveByVel d =
            let
                (Point x y) =
                    d.pos

                (Velocity vx vy) =
                    d.vel
            in
            { d | pos = Point (x + vx) (y + vy) }
    in
    data |> (updateVel >> moveByVel)



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
