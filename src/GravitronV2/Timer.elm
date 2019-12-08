module GravitronV2.Timer exposing
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


type Timer tag
    = Timer
        -- start
        Float
        -- duration
        Float
        --delay
        Float


start : Float -> Float -> Timer tag
start startedAt_ duration =
    Timer startedAt_ duration 0


delayedStart : Float -> Float -> Float -> Timer tag
delayedStart start_ duration_ delay_ =
    Timer start_ duration_ delay_


elapsed : Float -> Timer tag -> Float
elapsed clock (Timer st _ delay) =
    clock - st - delay |> max 0


isDone : Float -> Timer tag -> Bool
isDone clock ((Timer _ dur _) as model) =
    elapsed clock model > dur


restartIfDone : Float -> Timer tag -> Timer tag
restartIfDone clock =
    when (isDone clock) (restart clock)


restart : Float -> Timer tag -> Timer tag
restart clock (Timer _ dur _) =
    Timer clock dur 0


value : Float -> Timer tag -> Float
value clock ((Timer _ dur _) as model) =
    elapsed clock model / dur |> clamp 0 1


getStart : Timer tag -> Float
getStart (Timer st _ _) =
    st


setDuration : Float -> Timer tag -> Timer tag
setDuration newDuration (Timer st _ de) =
    Timer st newDuration de
