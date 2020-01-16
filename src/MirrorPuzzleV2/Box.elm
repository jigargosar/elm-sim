module MirrorPuzzleV2.Box exposing
    ( Box
    , center
    , contains
    , containsXY
    , dimensions
    , move
    , moveDown
    , moveRight
    , moveX
    , moveY
    , withWH
    )

import Number2 as NT


type Box
    = Box NT.Float2 NT.Float2


withWH : Float -> Float -> Box
withWH w h =
    Box ( 0, 0 ) ( w, h )


move : NT.Float2 -> Box -> Box
move by (Box pos dim) =
    Box (NT.add pos by) dim


moveX : Float -> Box -> Box
moveX dx =
    move ( dx, 0 )


moveDown : Float -> Box -> Box
moveDown dy =
    moveY -dy


moveRight : Float -> Box -> Box
moveRight =
    moveX


moveY : Float -> Box -> Box
moveY dy =
    move ( 0, dy )


center : Box -> NT.Float2
center (Box pos _) =
    pos


dimensions : Box -> ( Float, Float )
dimensions (Box _ dim) =
    dim


contains : NT.Float2 -> Box -> Bool
contains ( x, y ) (Box pos dim) =
    let
        halfDim =
            NT.scale 0.5 dim

        ( minX, minY ) =
            NT.sub pos halfDim

        ( maxX, maxY ) =
            NT.add pos halfDim
    in
    NT.contains x ( minX, maxX ) && NT.contains y ( minY, maxY )


containsXY : { a | x : Float, y : Float } -> Box -> Bool
containsXY { x, y } =
    contains ( x, y )
