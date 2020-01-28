module String2 exposing (fromFloat, join)

import PointFree exposing (mapEach)


type alias String2 =
    ( String, String )


fromFloat : ( Float, Float ) -> ( String, String )
fromFloat =
    mapEach String.fromFloat


join : String -> ( String, String ) -> String
join sep ( a, b ) =
    a ++ sep ++ b
