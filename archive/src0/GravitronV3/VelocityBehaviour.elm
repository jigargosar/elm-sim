module GravitronV3.VelocityBehaviour exposing
    ( VelocityBehaviour
    , initWanderAndBounceInScreen
    , update
    , updateRecord
    )

import GravitronV3.Screen exposing (Screen)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import List.Extra
import Random exposing (Generator, Seed)
import Random.Float


type alias Env =
    { mousePosition : Vec
    , screen : Screen
    , clock : Float
    }


type VelocityBehaviour
    = Wander Seed
    | BounceInScreen Float
    | Batch (List VelocityBehaviour)


initWanderAndBounceInScreen : Float -> VelocityBehaviour
initWanderAndBounceInScreen factor =
    Batch [ Wander (Random.initialSeed 0), BounceInScreen factor ]


randomWalkerVelocity : Vec -> Generator Vec
randomWalkerVelocity velocity =
    let
        randomAngle : Generator Float
        randomAngle =
            Random.Float.standardNormal |> Random.map ((*) 0.005 >> turns)
    in
    randomAngle
        |> Random.map
            (\newAngleDiff ->
                velocity
                    |> Vec.mapAngle ((+) newAngleDiff)
                    |> Vec.mapMagnitude (max 0.01)
            )


update : Env -> { a | position : Vec, velocity : Vec } -> VelocityBehaviour -> ( Vec, VelocityBehaviour )
update { screen } { position, velocity } model =
    update2 screen position velocity model


type alias Record a =
    { a | position : Vec, velocity : Vec, velocityBehaviour : VelocityBehaviour }


updateRecord : Env -> Record a -> Record a
updateRecord env record =
    let
        ( newVelocity, newBehaviour ) =
            update env record record.velocityBehaviour
    in
    { record
        | velocity = newVelocity
        , velocityBehaviour = newBehaviour
        , position = Vec.add record.position record.velocity
    }


update2 : Screen -> Vec -> Vec -> VelocityBehaviour -> ( Vec, VelocityBehaviour )
update2 screen position velocity model =
    case model of
        Wander seed ->
            Random.step (randomWalkerVelocity velocity) seed
                |> Tuple.mapSecond Wander

        BounceInScreen bounceFactor ->
            ( bounceWithinScreen screen
                position
                bounceFactor
                velocity
            , model
            )

        Batch list ->
            List.Extra.mapAccuml (update2 screen position) velocity list
                |> Tuple.mapSecond Batch


bounceWithinScreen : Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreen screen position bounceFactor velocity =
    let
        bounceVelocityPart lo high positionPart velocityPart =
            if
                (positionPart < lo && velocityPart < 0)
                    || (positionPart > high && velocityPart > 0)
            then
                negate velocityPart

            else
                velocityPart

        ( x, y ) =
            Vec.toTuple position

        ( vx, vy ) =
            Vec.toTuple velocity

        newBouncedVelocity =
            vec (bounceVelocityPart screen.left screen.right x vx)
                (bounceVelocityPart screen.top screen.bottom y vy)
    in
    if velocity /= newBouncedVelocity then
        newBouncedVelocity |> Vec.mapMagnitude ((*) bounceFactor)

    else
        newBouncedVelocity
