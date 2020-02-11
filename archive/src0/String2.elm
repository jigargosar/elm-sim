module String2 exposing (fromFloat, fromInt, join, wrapJoin)

import PointFree exposing (mapEach)


type alias String2 =
    ( String, String )


fromFloat : ( Float, Float ) -> ( String, String )
fromFloat =
    mapEach String.fromFloat


fromInt : ( Int, Int ) -> ( String, String )
fromInt =
    mapEach String.fromInt


join : String -> ( String, String ) -> String
join sep ( a, b ) =
    a ++ sep ++ b


wrapJoin : String -> String -> String -> ( String, String ) -> String
wrapJoin prefix sep suffix ( a, b ) =
    prefix ++ a ++ sep ++ b ++ suffix
