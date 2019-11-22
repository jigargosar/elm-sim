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
repeat rowCount columnCount cellData =
    Inner rowCount columnCount (Array.repeat (rowCount * columnCount) cellData)
        |> Matrix


generator : Int -> Int -> Generator a -> Generator (Matrix a)
generator rowCount columnCount =
    Random.list (rowCount * columnCount)
        >> Random.map (Array.fromList >> Inner rowCount columnCount >> Matrix)


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap func (Matrix { rowCount, columnCount, data }) =
    Array.indexedMap (\i -> func (i // rowCount) (remainderBy columnCount i)) data
        |> Inner rowCount columnCount
        |> Matrix


mapAt : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
mapAt rowIndex columnIndex func (Matrix inner) =
    let
        idx =
            rowIndex * inner.rowCount + columnIndex
    in
    case Array.get idx inner.data of
        Nothing ->
            Matrix inner

        Just a ->
            Matrix { inner | data = Array.set idx (func a) inner.data }


toList : Matrix b -> List b
toList (Matrix { data }) =
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


get : Int -> Int -> Matrix a -> Maybe a
get rowIndex columnIndex (Matrix { rowCount, columnCount, data }) =
    if isIndexOutOfBounds rowIndex rowCount || isIndexOutOfBounds columnIndex columnCount then
        Nothing

    else
        Array.get (rowIndex * rowCount + columnIndex) data


isIndexOutOfBounds : Int -> Int -> Bool
isIndexOutOfBounds index length =
    index < 0 || index >= length
