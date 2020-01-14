module MirrorPuzzleV2.Rect exposing (Rect, fromWH, fromXYWH)


type Rect
    = Rect Float Float Float Float


fromWH : Float -> Float -> Rect
fromWH w h =
    fromXYWH 0 0 w h


fromXYWH : Float -> Float -> Float -> Float -> Rect
fromXYWH x y w h =
    Rect x y w h
