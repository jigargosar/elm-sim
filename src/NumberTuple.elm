module NumberTuple exposing (..)

import PointFree exposing (mapEach, mapEach2, mulBy)


type alias Int =
    ( Int, Int )


type alias Float =
    ( Float, Float )


scale : number -> ( number, number ) -> ( number, number )
scale factor =
    mapEach (mulBy factor)


add : ( number, number ) -> ( number, number ) -> ( number, number )
add =
    mapEach2 (+)


sub : ( number, number ) -> ( number, number ) -> ( number, number )
sub =
    mapEach2 (-)


mul : ( number, number ) -> ( number, number ) -> ( number, number )
mul =
    mapEach2 (*)


toFloat : Int -> Float
toFloat =
    mapEach Basics.toFloat


round : Float -> Int
round =
    mapEach Basics.round
