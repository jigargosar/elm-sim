module GravitronV2.Timer exposing (Timer, isDone, restart, start, startWithDelay, value)


type alias Timer =
    { start : Float
    , duration : Float
    , delay : Float
    }


start : Float -> Float -> Timer
start startedAt_ duration =
    { start = startedAt_
    , duration = duration
    , delay = 0
    }


startWithDelay : Float -> Float -> Float -> Timer
startWithDelay start_ duration_ delay_ =
    { start = start_, duration = duration_, delay = delay_ }


elapsed : Float -> Timer -> Float
elapsed clock model =
    clock - model.start |> max 0


isDone : Float -> Timer -> Bool
isDone clock model =
    elapsed clock model > model.duration


restart : Float -> Timer -> Timer
restart clock model =
    { model | start = clock }


value : Float -> Timer -> Float
value clock model =
    elapsed clock model / model.duration |> clamp 0 1
