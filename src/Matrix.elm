module Matrix exposing
    ( Matrix
    , generator
    , getWarped
    , indexedMap
    , mapAt
    , repeat
    , size
    , toList
    )

import Array exposing (Array)
import Random exposing (Generator)


type Matrix a
    = Matrix (Inner a)


type alias Inner a =
    { rc : Int
    , cc : Int
    , arr : Array a
    }


size : Matrix a -> ( Int, Int )
size (Matrix { rc, cc }) =
    ( rc, cc )


repeat : Int -> Int -> a -> Matrix a
repeat rc cc a =
    Inner rc cc (Array.repeat (rc * cc) a)
        |> Matrix


generator : Int -> Int -> Generator a -> Generator (Matrix a)
generator rc cc =
    Random.list (rc * cc)
        >> Random.map (Array.fromList >> Inner rc cc >> Matrix)


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap func (Matrix { rc, cc, arr }) =
    Inner rc cc (Array.indexedMap (\i -> func (i // rc) (remainderBy cc i)) arr)
        |> Matrix


mapAt : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
mapAt ri ci func (Matrix inner) =
    let
        idx =
            ri * inner.rc + ci
    in
    case Array.get idx inner.arr of
        Nothing ->
            Matrix inner

        Just a ->
            Matrix { inner | arr = Array.set idx (func a) inner.arr }


toList : Matrix b -> List b
toList (Matrix { rc, cc, arr }) =
    Array.toList arr


getWarped : Int -> Int -> Matrix c -> Maybe c
getWarped rowNum_ colNum_ (Matrix { rc, cc, arr }) =
    let
        rowNum =
            modBy rc rowNum_

        colNum =
            modBy rc colNum_
    in
    Array.get (rowNum * rc + colNum) arr
