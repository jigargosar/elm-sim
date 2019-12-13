module GravitronV3.RigidBody exposing (RigidBody, step, stepSeed)

import GravitronV3.Point as Point exposing (Point)
import GravitronV3.Vec exposing (Vec)
import Random exposing (Generator, Seed)


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


type alias RigidBodyWithSeed a =
    RigidBody { a | seed : Seed }


stepSeed : Generator (RigidBodyWithSeed a -> Vec) -> RigidBodyWithSeed a -> RigidBodyWithSeed a
stepSeed gen model =
    let
        ( func, newSeed ) =
            Random.step gen model.seed
    in
    { model | seed = newSeed }
        |> step [ func ]


stepPosition : RigidBody a -> RigidBody a
stepPosition model =
    { model | position = Point.moveBy model.velocity model.position }


updateVelocity : (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
updateVelocity func model =
    { model | velocity = func model }
