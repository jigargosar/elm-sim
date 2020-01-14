module MirrorPuzzleV2.Rect exposing
    ( Rect
    , center
    , contains
    , dimensions
    , fromWH
    , fromXYWH
    )


type Rect
    = Rect Float Float Float Float


fromWH : Float -> Float -> Rect
fromWH w h =
    fromXYWH 0 0 w h


fromXYWH : Float -> Float -> Float -> Float -> Rect
fromXYWH x y w h =
    Rect x y w h


center : Rect -> ( Float, Float )
center (Rect x y _ _) =
    ( x, y )


dimensions : Rect -> ( Float, Float )
dimensions (Rect _ _ w h) =
    ( w, h )


contains : ( Float, Float ) -> Rect -> Bool
contains ( px, py ) (Rect x y w h) =
    let
        ( minX, maxX ) =
            ( x - w / 2, x + w / 2 )

        ( minY, maxY ) =
            ( y - h / 2, y + h / 2 )
    in
    px > minX && px < maxX && py > minY && py < maxY
