module GravitronV2.Clock exposing (Clock, init, step)


type Clock tag
    = Clock Float


init : Clock tag
init =
    Clock 0


step : Clock tag -> Clock tag
step (Clock num) =
    num + 1 |> Clock
