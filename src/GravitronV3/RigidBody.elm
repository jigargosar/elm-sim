module GravitronV3.RigidBody exposing (RigidBody, step, stepWithSeed)

import GravitronV3.Point as Point exposing (Point)
import GravitronV3.Vec exposing (Vec)
import Random exposing (Generator, Seed)


type alias RigidBody a =
    { a
        | position : Point
        , velocity : Vec
    }


step : List (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
step funcList =
    stepVelocity funcList >> updatePosition


stepVelocity : List (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
stepVelocity funcLst model =
    List.foldl updateVelocity model funcLst


type alias RigidBodyWithSeed a =
    RigidBody { a | seed : Seed }


stepWithSeed : (RigidBodyWithSeed a -> Generator Vec) -> RigidBodyWithSeed a -> RigidBodyWithSeed a
stepWithSeed gen =
    updateVelocityWithSeed gen
        >> updatePosition


stepVelocityWithSeed :
    List (RigidBodyWithSeed a -> Generator Vec)
    -> RigidBodyWithSeed a
    -> RigidBodyWithSeed a
stepVelocityWithSeed funcList model =
    List.foldl updateVelocityWithSeed model funcList


updatePosition : RigidBody a -> RigidBody a
updatePosition model =
    { model | position = Point.moveBy model.velocity model.position }


updateVelocity : (RigidBody a -> Vec) -> RigidBody a -> RigidBody a
updateVelocity func model =
    { model | velocity = func model }


updateVelocityWithSeed :
    (RigidBodyWithSeed a -> Generator Vec)
    -> RigidBodyWithSeed a
    -> RigidBodyWithSeed a
updateVelocityWithSeed genFunc model =
    let
        ( newVelocity, newSeed ) =
            Random.step (genFunc model) model.seed
    in
    { model | velocity = newVelocity, seed = newSeed }
