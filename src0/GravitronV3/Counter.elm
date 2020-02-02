module GravitronV3.Counter exposing
    ( Counter
    , cycleStep
    , init
    , initDelayedBy
    , isDone
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


cycleStep : Counter -> ( Bool, Counter )
cycleStep ct =
    if ct.current - ct.delay >= ct.maxSteps then
        ( True, { ct | current = 0, delay = 0 } )

    else
        ( False, { ct | current = ct.current + 1 } )


step : Counter -> Counter
step ct =
    { ct | current = ct.current + 1 |> clamp 0 ct.maxSteps }


isDone : Counter -> Bool
isDone ct =
    progress ct == 1


progress : Counter -> Float
progress ct =
    if ct.current - ct.delay <= 0 then
        0

    else if ct.current - ct.delay >= ct.maxSteps then
        1

    else
        toFloat (ct.current - ct.delay) / toFloat ct.maxSteps
