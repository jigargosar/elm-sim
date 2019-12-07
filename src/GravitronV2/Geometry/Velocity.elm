module GravitronV2.Geometry.Velocity exposing (Velocity, fromToScaled, scale, zero)

import GravitronV2.Geometry.Internal as Internal exposing (Location)
import GravitronV2.Vec as Vec exposing (Vec)


type alias Velocity =
    Internal.Velocity


wrap : Vec -> Velocity
wrap =
    Internal.Velocity


unwrap : Velocity -> Vec
unwrap (Internal.Velocity vec) =
    vec


map : (Vec -> Vec) -> Velocity -> Velocity
map func =
    unwrap >> func >> wrap


zero : Velocity
zero =
    wrap Vec.zero


fromToScaled : Location -> Location -> Float -> Velocity
fromToScaled (Location p1) (Location p2) scaleBy =
    Vec.fromPt p1 p2 |> Vec.multiply scaleBy |> wrap


scale : Float -> Velocity -> Velocity
scale by =
    map <| Vec.multiply by
