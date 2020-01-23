module Length2 exposing (fold, member, toDict)

import Dict exposing (Dict)
import Number2 exposing (Int2)


fold : (Int2 -> b -> b) -> b -> Int2 -> b
fold func acc0 ( w, h ) =
    List.range 0 (h - 1)
        |> List.foldl
            (\y acc1 ->
                List.range 0 (w - 1)
                    |> List.foldl (\x -> func ( x, y )) acc1
            )
            acc0


toDict : (Int2 -> b) -> Int2 -> Dict Int2 b
toDict func =
    fold (\i2 -> Dict.insert i2 (func i2)) Dict.empty


member : Int2 -> Int2 -> Bool
member ( x, y ) ( w, h ) =
    isIndexMemberOf w x && isIndexMemberOf h y


isIndexMemberOf : number -> number -> Bool
isIndexMemberOf length index =
    index >= 0 && index <= length
