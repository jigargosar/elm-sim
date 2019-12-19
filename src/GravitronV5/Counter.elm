module GravitronV5.Counter exposing (Counter, cycle, done, fromMax, progress)


type Counter
    = Counter Int Int


fromMax : Int -> Counter
fromMax =
    Counter 0 << max 1


cycle : Counter -> Counter
cycle (Counter n mx) =
    Counter (n + 1 |> modBy mx) mx


done : Counter -> Bool
done (Counter n mx) =
    n == mx - 1


progress : Counter -> Float
progress (Counter n mx) =
    if n == 0 then
        0

    else
        toFloat n / toFloat mx
