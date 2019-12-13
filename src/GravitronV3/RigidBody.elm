module GravitronV3.RigidBody exposing (RigidBody, stepVelocity)

import GravitronV3.Point exposing (Point)
import GravitronV3.Vec exposing (Vec)


type alias RigidBody a =
    { a
        | position : Point
        , velocity : Vec
    }


stepVelocity : (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
stepVelocity func model =
    { model | velocity = func model }
