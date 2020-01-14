module MirrorPuzzleV2.Direction8 exposing (Direction8, fromInt, rotate, stepPos, toDegrees)


type alias Pos =
    ( Int, Int )


type Direction8
    = Dir Int


fromInt : Int -> Direction8
fromInt ct =
    Dir (modBy 8 ct)


rotate : Int -> Direction8 -> Direction8
rotate ct (Dir org) =
    fromInt (org + ct)


toDegrees : Direction8 -> Float
toDegrees (Dir ct) =
    45 * ct |> toFloat


stepPos : Direction8 -> Pos -> Pos
stepPos (Dir ct) ( x, y ) =
    let
        ( dx, dy ) =
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
    in
    ( x + dx, y + dy )
