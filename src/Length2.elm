module Length2 exposing (toDict)

import Dict exposing (Dict)
import Number2 as NT exposing (Int2)


toDict : (Int2 -> b) -> Int2 -> Dict Int2 b
toDict func =
    NT.fold (\i2 -> Dict.insert i2 (func i2)) Dict.empty
