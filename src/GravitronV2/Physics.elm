module GravitronV2.Physics exposing
    ( Location
    , Velocity
    , locationFromXY
    , locationZero
    , velocityFromToLocationScaled
    , velocityZero
    )


type Location
    = Location Float Float


type Velocity
    = Velocity Magnitude Angle


type Angle
    = Angle Float


type Magnitude
    = Magnitude Float


locationFromXY : Float -> Float -> Location
locationFromXY =
    Location


locationZero : Location
locationZero =
    Location 0 0


angleZero : Angle
angleZero =
    Angle 0


magnitudeZero : Magnitude
magnitudeZero =
    Magnitude 0


velocityZero : Velocity
velocityZero =
    Velocity magnitudeZero angleZero


velocityFromToLocationScaled : Location -> Location -> Float -> Velocity
velocityFromToLocationScaled (Location x1 y1) (Location x2 y2) scale =
    let
        dx =
            x2 - x1

        dy =
            y2 - y1

        magnitude =
            dx ^ 2 + dy ^ 2 |> sqrt |> (*) scale |> Magnitude

        angle =
            atan2 dy dx |> Angle
    in
    Velocity magnitude angle
