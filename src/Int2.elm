module Int2 exposing (..)

import Number2 exposing (Int2)
import PointFree exposing (mapEach)


toString =
    mapEach String.fromInt >> asList >> String.join ","


asList : ( b, b ) -> List b
asList ( a, b ) =
    [ a, b ]
