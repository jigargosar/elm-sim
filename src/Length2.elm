module Length2 exposing (member, toDict)

import Dict exposing (Dict)
import Number2 as NT exposing (Int2)


toDict : (Int2 -> b) -> Int2 -> Dict Int2 b
toDict func =
    NT.fold (\i2 -> Dict.insert i2 (func i2)) Dict.empty


member : Int2 -> Int2 -> Bool
member ( x, y ) ( w, h ) =
    isIndexMemberOf w x && isIndexMemberOf h y


isIndexMemberOf : number -> number -> Bool
isIndexMemberOf length index =
    index >= 0 && index <= length
