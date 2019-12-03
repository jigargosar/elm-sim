module GravitronV2.Timer exposing (Timer)


type alias Timer =
    { startedAt : Float
    , duration : Float
    }


start : Float -> Timer
start clock =
    { startedAt
    }


step : Timer -> Timer
step model =
    { model
        | elapsed = model.elapsed + 1
    }

isDone =

