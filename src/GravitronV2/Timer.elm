module GravitronV2.Timer exposing (Timer, isDone, restart, start, value)


type alias Timer =
    { startedAt : Float
    , duration : Float
    }


start : Float -> Float -> Timer
start startedAt_ duration =
    { startedAt = startedAt_
    , duration = duration
    }


elapsed : Float -> Timer -> Float
elapsed clock model =
    clock - model.startedAt |> max 0


isDone : Float -> Timer -> Bool
isDone clock model =
    elapsed clock model > model.duration


restart : Float -> Timer -> Timer
restart clock model =
    { model | startedAt = clock }


value : Float -> Timer -> Float
value clock model =
    elapsed clock model / model.duration |> clamp 0 1
