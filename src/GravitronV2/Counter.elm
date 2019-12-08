module GravitronV2.Counter exposing (Counter, init, step)


type alias Counter =
    { num : Int
    , maxSteps : Int
    }


init : Int -> Counter
init maxSteps_ =
    { num = 0, maxSteps = maxSteps_ }


step : Counter -> ( Bool, Counter )
step counter =
    let
        nextNum =
            counter.num + 1
    in
    if nextNum >= counter.maxSteps then
        ( True, { counter | num = 0 } )

    else
        ( False, { counter | num = nextNum } )
