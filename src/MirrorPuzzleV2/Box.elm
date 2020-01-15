module MirrorPuzzleV2.Box exposing
    ( Box
    , center
    , contains
    , containsXY
    , dimensions
    , fromWH
    , fromXYWH
    , move
    , moveDown
    , moveRight
    , moveX
    , moveY
    )

import NumberTuple as NT


type Box
    = Box NT.Float Float Float


fromWH : Float -> Float -> Box
fromWH w h =
    fromXYWH 0 0 w h


move : NT.Float -> Box -> Box
move by (Box pos w h) =
    Box (NT.add pos by) w h


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


fromXYWH : Float -> Float -> Float -> Float -> Box
fromXYWH x y w h =
    Box ( x, y ) w h


center : Box -> NT.Float
center (Box pos _ _) =
    pos


dimensions : Box -> ( Float, Float )
dimensions (Box _ w h) =
    ( w, h )


contains : ( Float, Float ) -> Box -> Bool
contains pt (Box pos w h) =
    let
        dim =
            ( w, h )

        halfDim =
            NT.scale 0.5 dim

        pMin =
            NT.sub pos halfDim

        pMax =
            NT.add pos halfDim
    in
    (NT.lt pt pMin || NT.gt pt pMax)
        |> not


containsXY : { a | x : Float, y : Float } -> Box -> Bool
containsXY { x, y } =
    contains ( x, y )
