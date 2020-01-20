module MirrorPuzzleV2.MouseEvent exposing (..)

import Number2 exposing (Float2)


type MouseEvent
    = Click Float2
    | DragStart Float2
    | Drag Float2 Float2
    | Drop Float2 Float2
    | None


onClick : MouseEvent -> (Float2 -> Maybe a) -> Maybe a
onClick ev func =
    case ev of
        Click pt ->
            func pt

        _ ->
            Nothing
