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
    { rowCount : Int
    , columnCount : Int
    , data : Array a
    }


size : Matrix a -> ( Int, Int )
size (Matrix { rowCount, columnCount }) =
    ( rowCount, columnCount )


repeat : Int -> Int -> a -> Matrix a
repeat rc cc a =
    Inner rc cc (Array.repeat (rc * cc) a)
        |> Matrix


generator : Int -> Int -> Generator a -> Generator (Matrix a)
generator rc cc =
    Random.list (rc * cc)
        >> Random.map (Array.fromList >> Inner rc cc >> Matrix)


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap func (Matrix { rowCount, columnCount, data }) =
    Inner rowCount columnCount (Array.indexedMap (\i -> func (i // rowCount) (remainderBy columnCount i)) data)
        |> Matrix


mapAt : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
mapAt ri ci func (Matrix inner) =
    let
        idx =
            ri * inner.rowCount + ci
    in
    case Array.get idx inner.data of
        Nothing ->
            Matrix inner

        Just a ->
            Matrix { inner | data = Array.set idx (func a) inner.data }


toList : Matrix b -> List b
toList (Matrix { rowCount, columnCount, data }) =
    Array.toList data


getWarped : Int -> Int -> Matrix c -> Maybe c
getWarped rowNum_ colNum_ (Matrix { rowCount, columnCount, data }) =
    let
        rowNum =
            modBy rowCount rowNum_

        colNum =
            modBy rowCount colNum_
    in
    Array.get (rowNum * rowCount + colNum) data
