module GravitronV2.Timer exposing (Timer, delayedStart, isDone, restart, start, value)


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


delayedStart : Float -> Float -> Float -> Timer
delayedStart start_ duration_ delay_ =
    { start = start_, duration = duration_, delay = delay_ }


elapsed : Float -> Timer -> Float
elapsed clock model =
    clock - model.start - model.delay |> max 0


isDone : Float -> Timer -> Bool
isDone clock model =
    elapsed clock model > model.duration


restart : Float -> Timer -> Timer
restart clock model =
    { model | start = clock, delay = 0 }


value : Float -> Timer -> Float
value clock model =
    elapsed clock model / model.duration |> clamp 0 1
