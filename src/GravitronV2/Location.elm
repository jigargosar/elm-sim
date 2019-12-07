module GravitronV2.Location exposing (Location, xy, zero)

import GravitronV2.Physics as Physics


type alias Location =
    Physics.Location


xy : Float -> Float -> Location
xy =
    Physics.locationFromXY


zero : Location
zero =
    Physics.locationZero
