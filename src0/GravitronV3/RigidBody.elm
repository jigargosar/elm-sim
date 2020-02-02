module GravitronV3.RigidBody exposing (Circular, CircularBody, RigidBody, doCircleOverlap, step, stepWithSeed)

import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.Vec as Vec exposing (Vec)
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


stepWithSeed :
    List (RigidBodyWithSeed a -> Generator Vec)
    -> RigidBodyWithSeed a
    -> RigidBodyWithSeed a
stepWithSeed gen =
    stepVelocityWithSeed gen >> updatePosition


stepVelocityWithSeed :
    List (RigidBodyWithSeed a -> Generator Vec)
    -> RigidBodyWithSeed a
    -> RigidBodyWithSeed a
stepVelocityWithSeed funcList model =
    List.foldl updateVelocityWithSeed model funcList


updatePosition : RigidBody a -> RigidBody a
updatePosition model =
    { model | position = Pt.moveBy model.velocity model.position }


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


type alias Circular a =
    { a
        | position : Point
        , radius : Float
    }


type alias CircularBody a =
    RigidBody (Circular a)


doCircleOverlap : Circular a -> Circular b -> Bool
doCircleOverlap c1 c2 =
    Vec.lenFrom (c1.position |> Pt.toTuple |> Vec.fromTuple)
        (c2.position |> Pt.toTuple |> Vec.fromTuple)
        < (c1.radius + c2.radius - 1)
