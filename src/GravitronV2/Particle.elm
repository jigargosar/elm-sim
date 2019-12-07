module GravitronV2.Particle exposing (..)

import GravitronV2.Vec as Vec exposing (Vec)


type alias Particle a =
    { a
        | position : Vec
        , velocity : Vec
        , radius : Float
    }


fromToScaled : Vec -> Vec -> Float -> Vec
fromToScaled from to factor =
    Vec.fromPt from to |> Vec.multiply factor


scale : Float -> Vec -> Vec
scale =
    Vec.multiply
