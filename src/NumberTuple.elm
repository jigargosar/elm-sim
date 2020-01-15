module NumberTuple exposing (..)

import PointFree exposing (mapEach, mapEach2, mulBy)


type alias Int =
    ( Basics.Int, Basics.Int )


type alias Float =
    ( Basics.Float, Basics.Float )


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


div : Float -> Float -> Float
div =
    mapEach2 (/)


toFloat : Int -> Float
toFloat =
    mapEach Basics.toFloat


round : Float -> Int
round =
    mapEach Basics.round


negate : ( number, number ) -> ( number, number )
negate =
    mapEach Basics.negate


fromXY : { a | x : number, y : number } -> ( number, number )
fromXY { x, y } =
    ( x, y )
