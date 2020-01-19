module MirrorPuzzleV2.MouseEvent exposing (..)

import Number2 exposing (Float2)


type MouseEvent
    = Click Float2
    | DragStart Float2
    | Drag Float2 Float2
    | Drop Float2 Float2
    | None
