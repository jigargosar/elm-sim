module Playground.Direction8 exposing
    ( Direction8
    , down
    , fromInt
    , left
    , opposite
    , right
    , rotate
    , stepPos
    , stepPosIn
    , toDegrees
    , toRadians
    , up
    )

import Number2 as NT exposing (Int2)
import PointFree exposing (flip)


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


toRadians : Direction8 -> Float
toRadians (Dir ct) =
    turns (1 / 8) * toFloat ct


opposite : Direction8 -> Direction8
opposite =
    rotate 4


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


stepPosIn : Direction8 -> Int2 -> Int2
stepPosIn dir pos =
    NT.add pos (toVec dir)


stepPos : Int2 -> Direction8 -> Int2
stepPos =
    flip stepPosIn
