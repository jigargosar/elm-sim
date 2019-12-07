module GravitronV2.Velocity exposing (Velocity, betweenScaled, zero)

import GravitronV2.Physics as Physics exposing (Location)


type alias Velocity =
    Physics.Velocity


zero : Velocity
zero =
    Physics.velocityZero


betweenScaled : Location -> Location -> Float -> Velocity
betweenScaled =
    Physics.velocityFromToLocationScaled
