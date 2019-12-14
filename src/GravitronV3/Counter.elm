module GravitronV3.Counter exposing (Counter, init, step)


type alias Counter =
    { current : Int
    , maxSteps : Int
    }


init : Int -> Counter
init maxSteps_ =
    { current = 0, maxSteps = maxSteps_ }


step : Counter -> ( Bool, Counter )
step counter =
    if counter.current >= counter.maxSteps then
        ( True, { counter | current = 0 } )

    else
        ( False, { counter | current = counter.current + 1 } )


progress : Counter -> Float
progress ct =
    if ct.current == 0 then
        0

    else
        toFloat ct.maxSteps / toFloat ct.current
