module Number2 exposing (..)

import PointFree exposing (mapEach, mapEach2, mulBy)


type alias Int2 =
    ( Basics.Int, Basics.Int )


type alias Float2 =
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


div : Float2 -> Float2 -> Float2
div =
    mapEach2 (/)


toFloat : Int2 -> Float2
toFloat =
    mapEach Basics.toFloat


round : Float2 -> Int2
round =
    mapEach Basics.round


negate : ( number, number ) -> ( number, number )
negate =
    mapEach Basics.negate


fromXY : { a | x : number, y : number } -> ( number, number )
fromXY { x, y } =
    ( x, y )


contains : comparable -> ( comparable, comparable ) -> Bool
contains a ( minA, maxA ) =
    (a < minA || a > maxA)
        |> not
