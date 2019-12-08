module GravitronV2.Counter exposing (Counter, init, step)


type Counter tag
    = Counter Int Int


init : Int -> Counter tag
init =
    Counter 0


step : Counter tag -> ( Bool, Counter tag )
step (Counter num maxSteps) =
    let
        nextNum =
            num + 1
    in
    if nextNum >= maxSteps then
        ( True, Counter 0 maxSteps )

    else
        ( False, Counter nextNum maxSteps )
