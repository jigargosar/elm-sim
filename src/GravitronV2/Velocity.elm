module GravitronV2.Velocity exposing (Velocity, zero)


type Velocity
    = Velocity Magnitude Angle


type Angle
    = Angle Float


type Magnitude
    = Magnitude Float


zeroAngle : Angle
zeroAngle =
    Angle 0


zeroMagnitude : Magnitude
zeroMagnitude =
    Magnitude 0


zero : Velocity
zero =
    Velocity zeroMagnitude zeroAngle
