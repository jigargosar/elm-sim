module Grid exposing (Grid, empty, get, set)

import Dict exposing (Dict)
import Tuple exposing (mapBoth)


type alias GridDict a =
    Dict ( Int, Int ) a


type alias Grid a =
    { width : Int
    , height : Int
    , dict : GridDict a
    }


empty : Int -> Int -> Grid a
empty w h =
    Grid w h Dict.empty


set : Int -> Int -> a -> Grid a -> Grid a
set x y a =
    when (isValid x y)
        (setUnsafe x y a)


get : Int -> Int -> Grid a -> Maybe a
get x y =
    whenOr Nothing
        (isValid x y)
        (.dict >> Dict.get ( x, y ))


setUnsafe : Int -> Int -> a -> Grid a -> Grid a
setUnsafe x y a =
    with .dict (Dict.insert ( x, y ) a >> setDict)


setDict : GridDict a -> Grid a -> Grid a
setDict dict grid =
    { grid | dict = dict }


isValid : Int -> Int -> Grid a -> Bool
isValid x y =
    prop2 .width .height
        >> mapBoth (isValid_ x) (isValid_ y)
        >> join2 (&&)


isValid_ idx len =
    idx >= 0 && idx < len



-- POINT FREE


with p1 func val =
    func (p1 val) val


join2 func ( a, b ) =
    func a b


prop2 p1 p2 v =
    ( p1 v, p2 v )


when pred true val =
    if pred val then
        true val

    else
        val


whenOr default pred true val =
    if pred val then
        true val

    else
        default
