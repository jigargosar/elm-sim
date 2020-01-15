module MirrorPuzzleV2.Rect exposing
    ( Box
    , center
    , contains
    , dimensions
    , fromWH
    , fromXYWH
    )


type Box
    = Box Float Float Float Float


fromWH : Float -> Float -> Box
fromWH w h =
    fromXYWH 0 0 w h


fromXYWH : Float -> Float -> Float -> Float -> Box
fromXYWH x y w h =
    Box x y w h


center : Box -> ( Float, Float )
center (Box x y _ _) =
    ( x, y )


dimensions : Box -> ( Float, Float )
dimensions (Box _ _ w h) =
    ( w, h )


contains : ( Float, Float ) -> Box -> Bool
contains ( px, py ) (Box x y w h) =
    let
        ( minX, maxX ) =
            ( x - w / 2, x + w / 2 )

        ( minY, maxY ) =
            ( y - h / 2, y + h / 2 )
    in
    px > minX && px < maxX && py > minY && py < maxY
