module GravitronV5.Timer exposing (Timer, done, forTicks, progress, ticks)


type Timer
    = Timer Int Int


forTicks : Int -> Timer
forTicks =
    Timer 0 << max 1


ticks : Timer -> Timer
ticks (Timer n mx) =
    Timer (n + 1 |> modBy mx) mx


done : Timer -> Bool
done (Timer n mx) =
    n == mx - 1


progress : Timer -> Float
progress (Timer n mx) =
    if n == 0 then
        0

    else
        toFloat n / toFloat mx
