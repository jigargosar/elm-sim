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


type alias Matrix a =
    { rc : Int
    , cc : Int
    , arr : Array a
    }


size : Matrix a -> ( Int, Int )
size { rc, cc } =
    ( rc, cc )


repeat : Int -> Int -> a -> Matrix a
repeat rc cc a =
    Matrix rc cc (Array.repeat (rc * cc) a)


generator : Int -> Int -> Generator a -> Generator (Matrix a)
generator rc cc =
    Random.list (rc * cc)
        >> Random.map (Array.fromList >> Matrix rc cc)


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap func { rc, cc, arr } =
    Matrix rc cc (Array.indexedMap (\i -> func (i // rc) (remainderBy cc i)) arr)


mapAt : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
mapAt ri ci func mat =
    let
        idx =
            ri * mat.rc + ci
    in
    case Array.get idx mat.arr of
        Nothing ->
            mat

        Just a ->
            { mat | arr = Array.set idx (func a) mat.arr }


toList : Matrix b -> List b
toList =
    .arr >> Array.toList


getWarped : Int -> Int -> Matrix c -> Maybe c
getWarped rowNum_ colNum_ { rc, cc, arr } =
    let
        rowNum =
            if rowNum_ < 0 || rowNum_ >= rc then
                modBy rc rowNum_

            else
                rowNum_

        colNum =
            if colNum_ < 0 || colNum_ >= cc then
                modBy rc colNum_

            else
                colNum_
    in
    Array.get (rowNum * rc + colNum) arr
