module GravitronV3.RigidBody exposing (RigidBody, updateVelocity)

import GravitronV3.Point exposing (Point)
import GravitronV3.Vec exposing (Vec)


type alias RigidBody a =
    { a
        | position : Point
        , velocity : Vec
    }


updateVelocity : (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
updateVelocity func model =
    { model | velocity = func model }
