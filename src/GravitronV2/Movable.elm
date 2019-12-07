module GravitronV2.Movable exposing (..)

import GravitronV2.Vec exposing (Vec)


type alias Particle a =
    { a
        | position : Vec
        , velocity : Vec
        , radius : Float
    }
