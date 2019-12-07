module GravitronV2.Location exposing (Location, xy, zero)


type Location
    = Location Float Float


xy : Float -> Float -> Location
xy =
    Location


zero : Location
zero =
    Location 0 0
