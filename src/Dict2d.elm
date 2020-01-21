module Dict2d exposing (..)

import Dict exposing (Dict)
import Length2
import List2d exposing (List2d)
import Number2 exposing (Int2)


filled : a -> Int2 -> Dict Int2 a
filled a =
    Length2.toDict (always a)


resizeWithDefault : a -> Int2 -> Dict Int2 a -> Dict Int2 a
resizeWithDefault a len2 dict2d =
    let
        func index2 =
            Dict.get index2 dict2d |> Maybe.withDefault a
    in
    Length2.toDict func len2


fromList2dWithDefault : a -> List2d a -> ( Int2, Dict Int2 a )
fromList2dWithDefault a list2d =
    let
        lookupDict =
            List2d.toDict list2d

        length2 =
            ( List2d.maxWidth list2d, List2d.height list2d )
    in
    ( length2, resizeWithDefault a length2 lookupDict )


mapAt : Int2 -> (a -> a) -> Dict Int2 a -> Maybe (Dict Int2 a)
mapAt gIdx func grid =
    Dict.get gIdx grid
        |> Maybe.map (\cell -> Dict.insert gIdx (func cell) grid)


mapAt2 : Int2 -> Int2 -> (b -> b -> ( b, b )) -> Dict Int2 b -> Maybe (Dict Int2 b)
mapAt2 gIdxA gIdxB func grid =
    let
        insertCells ( newCellA, newCellB ) =
            grid
                |> Dict.insert gIdxA newCellA
                |> Dict.insert gIdxB newCellB
    in
    Maybe.map2
        (\cellA cellB -> func cellA cellB |> insertCells)
        (Dict.get gIdxA grid)
        (Dict.get gIdxB grid)
