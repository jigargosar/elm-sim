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
    Array.repeat (rowCount * columnCount) cellData
        |> Inner rowCount columnCount
        |> Matrix


generator : Int -> Int -> Generator a -> Generator (Matrix a)
generator rowCount columnCount =
    Random.list (rowCount * columnCount)
        >> Random.map
            (Array.fromList
                >> Inner rowCount columnCount
                >> Matrix
            )


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap func (Matrix { rowCount, columnCount, data }) =
    Array.indexedMap (\i -> func (i // rowCount) (remainderBy columnCount i)) data
        |> Inner rowCount columnCount
        |> Matrix


mapAt : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
mapAt rowIndex columnIndex func matrix =
    if areIndicesOutOfBounds rowIndex columnIndex matrix then
        matrix

    else
        let
            (Matrix inner) =
                matrix

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
getWarped rowIndex columnIndex ((Matrix { rowCount, columnCount }) as matrix) =
    getUnsafe
        (modBy rowCount rowIndex)
        (modBy columnCount columnIndex)
        matrix


getUnsafe : Int -> Int -> Matrix a -> Maybe a
getUnsafe rowIndex columnIndex (Matrix { rowCount, data }) =
    Array.get (rowIndex * rowCount + columnIndex) data


areIndicesOutOfBounds : Int -> Int -> Matrix a -> Bool
areIndicesOutOfBounds rowIndex columnIndex (Matrix { rowCount, columnCount }) =
    rowIndex < 0 || columnIndex < 0 || rowIndex >= rowCount || columnIndex >= columnCount
