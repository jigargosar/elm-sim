module Stack exposing (Stack, empty, foldFifo, push, pushAll, toFifo, toLifo)

import PointFree exposing (cons)


type Stack a
    = Stack (List a)


empty : Stack a
empty =
    Stack []


unwrap : Stack a -> List a
unwrap (Stack list) =
    list


map : (List a -> List b) -> Stack a -> Stack b
map func =
    unwrap >> func >> Stack


push : a -> Stack a -> Stack a
push a =
    map (cons a)


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
