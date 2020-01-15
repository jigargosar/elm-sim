module MirrorPuzzleV2.Box exposing
    ( Box
    , center
    , contains
    , dimensions
    , fromWH
    , fromXYWH
    , move
    , moveDown
    , moveRight
    , moveX
    , moveY
    )


type Box
    = Box Float Float Float Float


fromWH : Float -> Float -> Box
fromWH w h =
    fromXYWH 0 0 w h


move : Float -> Float -> Box -> Box
move dx dy (Box x y w h) =
    Box (x + dx) (y + dy) w h


moveX : Float -> Box -> Box
moveX dx =
    move dx 0


moveDown : Float -> Box -> Box
moveDown dy =
    moveY -dy


moveRight : Float -> Box -> Box
moveRight =
    moveX


moveY : Float -> Box -> Box
moveY dy =
    move 0 dy


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
