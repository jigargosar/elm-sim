module MirrorPuzzleV2.Box exposing
    ( Box
    , atOrigin
    , atTopLeft
    , atTopRight
    , center
    , contains
    , containsXY
    , dimensions
    , move
    , moveDown
    , moveLeft
    , moveRight
    , moveToTopLeft
    , moveToTopRight
    , moveX
    , moveY
    )

import Number2 as NT


type Box
    = Box NT.Float2 NT.Float2


atOrigin : Float -> Float -> Box
atOrigin w h =
    Box ( 0, 0 ) ( w, h )


atTopLeft : Float -> Float -> Box
atTopLeft w h =
    atOrigin w h
        |> moveRight (w / 2)
        |> moveDown (h / 2)


atTopRight : Float -> Float -> Box
atTopRight w h =
    atOrigin w h
        |> moveLeft (w / 2)
        |> moveDown (h / 2)


moveToTopLeft : Box -> Box
moveToTopLeft ((Box _ ( w, h )) as box) =
    box
        |> moveLeft (w / 2)
        |> moveDown (h / 2)


moveToTopRight : Box -> Box
moveToTopRight ((Box _ ( w, h )) as box) =
    box
        |> moveLeft (w / 2)
        |> moveDown (h / 2)


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


moveLeft : Float -> Box -> Box
moveLeft =
    negate >> moveX


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
