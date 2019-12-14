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
step ct =
    if ct.current - ct.delay >= ct.maxSteps then
        ( True, { ct | current = 0, delay = 0 } )

    else
        ( False, { ct | current = ct.current + 1 } )


progress : Counter -> Float
progress ct =
    if ct.current - ct.delay <= 0 then
        0

    else
        toFloat (ct.current - ct.delay) / toFloat ct.maxSteps
