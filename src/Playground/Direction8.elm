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
    , up
    )

import Number2 as NT
import PointFree exposing (flip)


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


toVecCW : Direction8 -> ( number, number )
toVecCW =
    toVec >> Tuple.mapSecond negate


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


stepPosIn : Direction8 -> Pos -> Pos
stepPosIn dir pos =
    NT.add pos (toVec dir)


stepPos : Pos -> Direction8 -> Pos
stepPos =
    flip stepPosIn
