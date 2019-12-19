module GravitronV5.Main exposing (main)

import Playground exposing (..)



-- Geom


type Point
    = Point Number Number


type Velocity
    = Velocity Number Number


moveByVel : { a | pos : Point, vel : Velocity } -> { a | pos : Point, vel : Velocity }
moveByVel data =
    let
        (Point x y) =
            data.pos

        (Velocity vx vy) =
            data.vel
    in
    { data | pos = Point (x + vx) (y + vy) }


updateVelocityRandomWalker : Time -> { a | vel : Velocity } -> { a | vel : Velocity }
updateVelocityRandomWalker time data =
    let
        (Velocity vx vy) =
            data.vel

        f =
            0.01
    in
    { data | vel = Velocity (vx + wave -f f 7 time) (vy + wave -f f 5 time) }



-- Actor


type Actor
    = Player Data


type Movement
    = RandomWalker


type Behaviour
    = Movement Movement


type alias Data =
    { tag : Tag
    , pos : Point
    , vel : Velocity
    , behaviours : List Behaviour
    }


type Tag
    = PlayerTag



-- Player


initialPlayer : Actor
initialPlayer =
    Player (Data PlayerTag (Point 0 0) (Velocity 0 0) [ Movement RandomWalker ])



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
updateActor computer actor =
    case actor of
        Player data ->
            List.foldl (applyBehaviour computer) data data.behaviours
                |> Player


applyBehaviour : Computer -> Behaviour -> Data -> Data
applyBehaviour { time } behaviour data =
    case behaviour of
        Movement mv ->
            let
                updateVelocity =
                    case mv of
                        RandomWalker ->
                            updateVelocityRandomWalker time
            in
            data |> updateVelocity >> moveByVel



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
