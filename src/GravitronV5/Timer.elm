module GravitronV5.Timer exposing (Timer, cycle, done, fromMax, progress)


type Timer
    = Timer Int Int


fromMax : Int -> Timer
fromMax =
    Timer 0 << max 1


cycle : Timer -> Timer
cycle (Timer n mx) =
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
