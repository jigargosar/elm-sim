module GravitronV3.RigidBody exposing (..)

import GravitronV3.Point exposing (Point)
import GravitronV3.Vec exposing (Vec)


type alias RigidBody a =
    { a
        | position : Point
        , velocity : Vec
    }
