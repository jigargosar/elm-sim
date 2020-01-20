module Number2 exposing (..)

import PointFree exposing (flip, mapEach, mapEach2, mulBy)


type alias Int2 =
    ( Basics.Int, Basics.Int )


type alias Float2 =
    ( Basics.Float, Basics.Float )


scale : number -> ( number, number ) -> ( number, number )
scale factor =
    mapEach (mulBy factor)


apply : (a -> b -> c) -> ( a, b ) -> c
apply func ( a, b ) =
    func a b


add : ( number, number ) -> ( number, number ) -> ( number, number )
add =
    mapEach2 (+)


sub : ( number, number ) -> ( number, number ) -> ( number, number )
sub =
    mapEach2 (-)


subBy : ( number, number ) -> ( number, number ) -> ( number, number )
subBy =
    flip sub


mul : ( number, number ) -> ( number, number ) -> ( number, number )
mul =
    mapEach2 (*)


div : Float2 -> Float2 -> Float2
div =
    mapEach2 (/)


divBy : Float2 -> Float2 -> Float2
divBy =
    flip div


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


equalWithin : number -> ( number, number ) -> ( number, number ) -> Bool
equalWithin tol a b =
    let
        ( dx, dy ) =
            sub a b |> mapEach abs
    in
    dx < tol && dy < tol


foldIndices2d : (Int2 -> b -> b) -> b -> Int2 -> b
foldIndices2d func acc0 ( w, h ) =
    List.range 0 (h - 1)
        |> List.foldl
            (\y acc1 ->
                List.range 0 (w - 1)
                    |> List.foldl (\x -> func ( x, y )) acc1
            )
            acc0
