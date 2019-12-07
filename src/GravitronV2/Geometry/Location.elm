module GravitronV2.Geometry.Location exposing (Location, xy, zero)

import GravitronV2.Geometry.Internal exposing (ILocation)
import GravitronV2.Vec as Vec exposing (Vec, vec)


type alias Location =
    ILocation


wrap : Vec -> Location
wrap =
    ILocation


xy : Float -> Float -> Location
xy =
    apply2Then vec wrap


apply2Then : (c -> d -> a) -> (a -> b) -> c -> d -> b
apply2Then func1 func2 a b =
    func1 a b |> func2


zero : Location
zero =
    Vec.zero |> wrap
