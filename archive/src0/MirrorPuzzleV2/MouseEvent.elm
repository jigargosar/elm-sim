module MirrorPuzzleV2.MouseEvent exposing (..)

import Number2 exposing (Float2)


type MouseEvent
    = Click Float2
    | DragStart Float2
    | Drag Float2 Float2
    | Drop Float2 Float2
    | None


onClick : (Float2 -> Maybe a) -> MouseEvent -> Maybe a
onClick func ev =
    case ev of
        Click pt ->
            func pt

        _ ->
            Nothing
