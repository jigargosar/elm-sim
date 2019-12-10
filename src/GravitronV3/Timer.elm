module GravitronV3.Timer exposing
    ( Timer
    , delayedStart
    , getStart
    , isDone
    , restart
    , restartIfDone
    , setDuration
    , start
    , value
    )

import PointFree exposing (when)


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


restartIfDone : Float -> Timer -> Timer
restartIfDone clock =
    when (isDone clock) (restart clock)


restart : Float -> Timer -> Timer
restart clock model =
    { model | start = clock, delay = 0 }


value : Float -> Timer -> Float
value clock model =
    elapsed clock model / model.duration |> clamp 0 1


getStart : Timer -> Float
getStart =
    .start


setDuration : Float -> Timer -> Timer
setDuration newDuration model =
    { model | duration = newDuration }
