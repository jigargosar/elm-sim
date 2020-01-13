module MirrorPuzzleV2.Direction8 exposing (Direction, fromInt, stepPos, toDegrees)


type alias Pos =
    ( Int, Int )


type Direction
    = Direction Int


fromInt : Int -> Direction
fromInt ct =
    Direction (modBy 8 ct)


toDegrees : Direction -> Float
toDegrees (Direction ct) =
    45 * ct |> toFloat


stepPos : Direction -> Pos -> Pos
stepPos (Direction ct) ( x, y ) =
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
