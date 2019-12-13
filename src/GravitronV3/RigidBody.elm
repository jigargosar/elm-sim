module GravitronV3.RigidBody exposing (RigidBody, step)

import GravitronV3.Point as Point exposing (Point)
import GravitronV3.Vec exposing (Vec)


type alias RigidBody a =
    { a
        | position : Point
        , velocity : Vec
    }


stepVelocity : List (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
stepVelocity funcLst model =
    List.foldl updateVelocity model funcLst


step : List (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
step funcList =
    stepVelocity funcList
        >> stepPosition


stepPosition : RigidBody a -> RigidBody a
stepPosition model =
    { model | position = Point.moveBy model.velocity model.position }


updateVelocity : (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
updateVelocity func model =
    { model | velocity = func model }
