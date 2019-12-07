module GravitronV2.Geometry.Point exposing (Point, fromVec__, xy, zero)

import GravitronV2.Geometry.Internal exposing (ILocation)
import GravitronV2.Vec as Vec exposing (Vec, vec)


type alias Point =
    ILocation


wrap : Vec -> Point
wrap =
    ILocation


xy : Float -> Float -> Point
xy =
    apply2Then vec wrap


apply2Then : (c -> d -> a) -> (a -> b) -> c -> d -> b
apply2Then func1 func2 a b =
    func1 a b |> func2


zero : Point
zero =
    Vec.zero |> wrap


fromVec__ : Vec -> Point
fromVec__ =
    wrap
