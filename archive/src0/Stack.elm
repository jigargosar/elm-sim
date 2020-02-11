module Stack exposing (Stack, empty, foldFifo, map, push, pushAll, pushOn, toFifo, toLifo)

import PointFree exposing (cons, consTo, flip)


type Stack a
    = Stack (List a)


empty : Stack a
empty =
    Stack []


unwrap : Stack a -> List a
unwrap (Stack list) =
    list


map_ : (List a -> List b) -> Stack a -> Stack b
map_ func =
    unwrap >> func >> Stack


push : a -> Stack a -> Stack a
push a =
    map_ (cons a)


pushOn : Stack a -> a -> Stack a
pushOn =
    flip push


pushAll : List a -> Stack a -> Stack a
pushAll items stack =
    List.foldl push stack items


foldFifo : (a -> b -> b) -> b -> Stack a -> b
foldFifo func acc =
    unwrap >> List.foldr func acc


toFifo : Stack a -> List a
toFifo =
    unwrap >> List.reverse


toLifo : Stack a -> List a
toLifo =
    unwrap


map : (a -> b) -> Stack a -> Stack b
map func =
    map_ (List.map func)
