module GravitronV2.Geometry.Velocity exposing (Velocity, add, fromToScaled, fromVec__, scale, toVec__, zero)

import GravitronV2.Geometry.Internal exposing (ILocation, IVelocity)
import GravitronV2.Vec as Vec exposing (Vec)


type alias Velocity =
    IVelocity


type alias Location =
    ILocation


wrap : Vec -> Velocity
wrap =
    IVelocity


unwrap : Velocity -> Vec
unwrap (IVelocity vec) =
    vec


map : (Vec -> Vec) -> Velocity -> Velocity
map func =
    unwrap >> func >> wrap


zero : Velocity
zero =
    wrap Vec.zero


fromToScaled : Location -> Location -> Float -> Velocity
fromToScaled (ILocation p1) (ILocation p2) scaleBy =
    Vec.fromPt p1 p2 |> Vec.multiply scaleBy |> wrap


add : Velocity -> Velocity -> Velocity
add v1 v2 =
    Vec.add (unwrap v1) (unwrap v2) |> wrap


scale : Float -> Velocity -> Velocity
scale by =
    map <| Vec.multiply by


fromVec__ : Vec -> Velocity
fromVec__ =
    wrap


toVec__ : Velocity -> Vec
toVec__ =
    unwrap
