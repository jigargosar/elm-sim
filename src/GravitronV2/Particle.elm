module GravitronV2.Particle exposing (..)

import GravitronV2.Vec as Vec exposing (Vec)


type alias HasPosition a =
    { a | position : Vec }


type alias Particle a =
    { a
        | position : Vec
        , velocity : Vec
        , radius : Float
    }


mapVelocity : (Vec -> Vec) -> Particle a -> Particle a
mapVelocity func model =
    { model | velocity = func model.velocity }


step : Particle a -> Particle a
step model =
    { model | position = Vec.add model.position model.velocity }
