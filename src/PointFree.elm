module PointFree exposing (..)

import Basics.Extra exposing (flip)
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


whenFalse : Bool -> (c -> c) -> c -> c
whenFalse bool =
    whenTrue (not bool)


pairedTo : b -> a -> ( a, b )
pairedTo =
    flip Tuple.pair


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach func =
    Tuple.mapBoth func func


subAB : number -> number -> number
subAB =
    (-)


subBA : number -> number -> number
subBA =
    flip subAB


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
