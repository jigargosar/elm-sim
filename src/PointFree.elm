module PointFree exposing (..)

import Basics.Extra exposing (flip, swap)
import List.Extra
import Random exposing (Generator, Seed)


with : (a -> b) -> (b -> a -> c) -> a -> c
with func1 func2 model =
    func2 (func1 model) model


when : (a -> Bool) -> (a -> a) -> a -> a
when p t v =
    if p v then
        t v

    else
        v


unless : (a -> Bool) -> (a -> a) -> a -> a
unless p =
    when (p >> not)


dec : number -> number
dec =
    (+) -1


clamp0 : number -> number -> number
clamp0 =
    clamp 0


appendAB : appendable -> appendable -> appendable
appendAB =
    (++)


appendBA : appendable -> appendable -> appendable
appendBA =
    flip appendAB


appendIf : Bool -> appendable -> appendable -> appendable
appendIf bool end start =
    if bool then
        start ++ end

    else
        start


whenTrue : Bool -> (c -> c) -> c -> c
whenTrue bool t v =
    if bool then
        t v

    else
        v


whenNothing : Maybe a -> (c -> c) -> c -> c
whenNothing maybe =
    whenTrue (maybe == Nothing)


inc : number -> number
inc =
    (+) 1


whenFalse : Bool -> (c -> c) -> c -> c
whenFalse bool =
    whenTrue (not bool)


pairedTo : b -> a -> ( a, b )
pairedTo =
    flip Tuple.pair


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach func =
    Tuple.mapBoth func func


toFloat2 : ( Int, Int ) -> ( Float, Float )
toFloat2 =
    mapEach toFloat


mod : Int -> Int -> Int
mod =
    flip modBy


findMapWithDefault : (a -> Maybe b) -> b -> List a -> b
findMapWithDefault func d =
    List.filterMap func
        >> List.head
        >> Maybe.withDefault d


findWithDefault : (b -> Bool) -> b -> List b -> b
findWithDefault pred d =
    List.Extra.find pred >> Maybe.withDefault d


stepRandomSeeded : Generator a -> { b | seed : Seed } -> ( a, { b | seed : Seed } )
stepRandomSeeded random model =
    Random.step random model.seed
        |> Tuple.mapSecond (\seed -> { model | seed = seed })


rejectWhen : (a -> Bool) -> List a -> List a
rejectWhen isNotOk =
    keepWhen (isNotOk >> not)


keepWhen : (a -> Bool) -> List a -> List a
keepWhen =
    List.filter


ifElse : (d -> Bool) -> (d -> a) -> (d -> a) -> d -> a
ifElse pred true false val =
    if pred val then
        true val

    else
        false val


anyPass : List (b -> Bool) -> b -> Bool
anyPass fnList val =
    List.any ((|>) val) fnList


allPass : List (b -> Bool) -> b -> Bool
allPass fnList val =
    List.all ((|>) val) fnList


propEq : (c -> b) -> b -> c -> Bool
propEq func a b =
    func b == a


flip : (c -> b -> a) -> b -> c -> a
flip func b a =
    func a b


cons : a -> List a -> List a
cons =
    (::)


consTo : List a -> a -> List a
consTo =
    flip cons


mapAccuml : (a -> acc -> ( b, acc )) -> acc -> List a -> ( acc, List b )
mapAccuml func acc =
    List.Extra.mapAccuml (\a b -> func b a |> swap) acc


pairTo : b -> a -> ( a, b )
pairTo =
    flip Tuple.pair


mulBy : number -> number -> number
mulBy =
    (*)


is =
    (==)


ignoreNothing : (b -> Maybe b) -> b -> b
ignoreNothing func val =
    func val |> Maybe.withDefault val


mapEach2 func ( a1, b1 ) ( a2, b2 ) =
    ( func a1 a2, func b1 b2 )


vecFromTo : ( number, number ) -> ( number, number ) -> ( number, number )
vecFromTo ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1, y2 - y1 )


apply2 func ( a, b ) =
    func a b


callWith : a -> (a -> b) -> b
callWith a func =
    func a
