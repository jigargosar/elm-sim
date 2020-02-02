module Dict2d exposing (decodeCSV, filled, fromListsWithDefault)

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
