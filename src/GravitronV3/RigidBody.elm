module GravitronV3.RigidBody exposing (RigidBody, stepVelocity)

import GravitronV3.Point exposing (Point)
import GravitronV3.Vec exposing (Vec)


type alias RigidBody a =
    { a
        | position : Point
        , velocity : Vec
    }


stepVelocity : List (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
stepVelocity funcLst model =
    List.foldl updateVelocity model funcLst


updateVelocity : (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
updateVelocity func model =
    { model | velocity = func model }
