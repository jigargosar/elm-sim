module Dict2d exposing (decodeCSV, filled, fromListsWithDefault, maybeMapAt, maybeMapAt2)

import Dict exposing (Dict)
import List2d exposing (List2d)
import Number2 as NT exposing (Int2)


filled : a -> Int2 -> Dict Int2 a
filled a =
    NT.toDict (always a)


resizeWithDefault : a -> Int2 -> Dict Int2 a -> Dict Int2 a
resizeWithDefault a length2 dict =
    let
        func idx =
            Dict.get idx dict |> Maybe.withDefault a
    in
    NT.toDict func length2


fromListsWithDefault : a -> List2d a -> ( Int2, Dict Int2 a )
fromListsWithDefault a lists =
    let
        lookupDict =
            List2d.toDict lists

        length2 =
            ( List2d.maxWidth lists, List2d.height lists )
    in
    ( length2, resizeWithDefault a length2 lookupDict )


decodeCSV : (String -> a) -> String -> ( Int2, Dict Int2 a )
decodeCSV decoder encoded =
    let
        tokens : List (List a)
        tokens =
            encoded
                |> String.trim
                >> String.lines
                >> List.reverse
                >> List.map (String.split "," >> List.map decoder)
    in
    fromListsWithDefault (decoder "") tokens


maybeMapAt : Int2 -> (a -> Maybe a) -> Dict Int2 a -> Maybe (Dict Int2 a)
maybeMapAt idx func dict =
    let
        replace a2 =
            Dict.insert idx a2 dict
    in
    Dict.get idx dict
        |> Maybe.andThen (func >> Maybe.map replace)


maybeMapAt2 : Int2 -> Int2 -> (a -> a -> Maybe ( a, a )) -> Dict Int2 a -> Maybe (Dict Int2 a)
maybeMapAt2 idxA1 idxA2 func dict =
    let
        getAt idx =
            Dict.get idx dict

        insert2 ( a1, a2 ) =
            dict
                |> Dict.insert idxA1 a1
                |> Dict.insert idxA2 a2
    in
    Maybe.map2 func (getAt idxA1) (getAt idxA2)
        |> Maybe.andThen identity
        |> Maybe.map insert2
