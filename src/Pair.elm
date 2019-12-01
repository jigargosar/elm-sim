module Pair exposing (Pair, from, map2)


type alias Pair a b =
    ( a, b )


from : a -> b -> Pair a b
from =
    Tuple.pair


map2 : (a -> b -> c) -> Pair a a -> Pair b b -> Pair c c
map2 func ( a1, b1 ) ( a2, b2 ) =
    from (func a1 a2) (func b1 b2)
