module GravitronV2.Geometry.Velocity exposing (Velocity, fromToScaled, scale, zero)

import GravitronV2.Geometry.Internal exposing (ILocation, IVelocity)
import GravitronV2.Vec as Vec exposing (Vec)


type alias Velocity =
    IVelocity


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


fromToScaled : ILocation -> ILocation -> Float -> Velocity
fromToScaled (ILocation p1) (ILocation p2) scaleBy =
    Vec.fromPt p1 p2 |> Vec.multiply scaleBy |> wrap


scale : Float -> Velocity -> Velocity
scale by =
    map <| Vec.multiply by
