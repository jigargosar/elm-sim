module GravitronV3.Counter exposing
    ( Counter
    , init
    , initDelayedBy
    , progress
    , step
    )


type alias Counter =
    { current : Int
    , maxSteps : Int
    , delay : Int
    }


init : Int -> Counter
init maxSteps_ =
    { current = 0, maxSteps = maxSteps_, delay = 0 }


initDelayedBy : Float -> Int -> Counter
initDelayedBy delayPct maxSteps_ =
    let
        delay =
            toFloat maxSteps_
                * delayPct
                |> round
    in
    { current = 0, maxSteps = maxSteps_, delay = delay }


step : Counter -> ( Bool, Counter )
step counter =
    if counter.current + counter.delay >= counter.maxSteps then
        ( True, { counter | current = 0 } )

    else
        ( False, { counter | current = counter.current + 1 } )


progress : Counter -> Float
progress ct =
    if ct.current + ct.delay <= 0 then
        0

    else
        toFloat ct.current / toFloat ct.maxSteps
