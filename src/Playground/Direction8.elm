module Playground.Direction8 exposing
    ( Direction8
    , down
    , fromInt
    , left
    , opposite
    , right
    , rotate
    , stepPos
    , stepPosCCW
    , toDegrees
    , up
    )

import Number2 as NT


type alias Pos =
    ( Int, Int )


type Direction8
    = Dir Int


fromInt : Int -> Direction8
fromInt ct =
    Dir (modBy 8 ct)


right : Direction8
right =
    fromInt 0


up : Direction8
up =
    rotate 2 right


left : Direction8
left =
    rotate 2 up


down : Direction8
down =
    rotate 2 left


rotate : Int -> Direction8 -> Direction8
rotate ct (Dir org) =
    fromInt (org + ct)


toDegrees : Direction8 -> Float
toDegrees (Dir ct) =
    45 * ct |> toFloat


opposite : Direction8 -> Direction8
opposite =
    rotate 4


stepPosCCW : Direction8 -> Pos -> Pos
stepPosCCW direction8 pos =
    stepPos direction8 (Tuple.mapSecond negate pos)
        |> Tuple.mapSecond negate


toVec : Direction8 -> ( number, number )
toVec (Dir ct) =
    case ct of
        0 ->
            ( 1, 0 )

        1 ->
            ( 1, 1 )

        2 ->
            ( 0, 1 )

        3 ->
            ( -1, 1 )

        4 ->
            ( -1, 0 )

        5 ->
            ( -1, -1 )

        6 ->
            ( 0, -1 )

        7 ->
            ( 1, -1 )

        _ ->
            ( 1, 0 )


stepPos : Direction8 -> Pos -> Pos
stepPos dir ( x, y ) =
    let
        ( dx, dy ) =
            toVec dir
    in
    ( x + dx, y + dy )
