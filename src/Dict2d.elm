module Dict2d exposing (..)

import Dict exposing (Dict)
import Length2
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
