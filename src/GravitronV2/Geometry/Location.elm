module GravitronV2.Geometry.Location exposing (Location, xy, zero)

import GravitronV2.Geometry.Internal as Internal
import GravitronV2.Vec as Vec exposing (Vec, vec)


type alias Location =
    Internal.Location


wrap : Vec -> Location
wrap =
    Internal.Location


xy : Float -> Float -> Location
xy =
    apply2Then vec wrap


apply2Then func1 func2 a b =
    func1 a b |> func2


zero : Location
zero =
    Vec.zero |> wrap
