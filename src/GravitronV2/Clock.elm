module GravitronV2.Clock exposing (Clock, init, step)


type Clock
    = Clock Float


init : Clock
init =
    Clock 0


step : Clock -> Clock
step (Clock num) =
    num + 1 |> Clock
