module GravitronV3.Point exposing (Point, moveBy, toTuple, vecFromTo, xy)

import GravitronV3.Vec as Vec exposing (Vec)


type Point
    = Point Vec


moveBy : Vec -> Point -> Point
moveBy vec (Point pt) =
    Point (Vec.add pt vec)


vecFromTo : Point -> Point -> Vec
vecFromTo (Point from) (Point to) =
    Vec.fromTo from to


xy : ( Float, Float ) -> Point
xy ( x, y ) =
    Point (Vec.vec x y)


toTuple : Point -> ( Float, Float )
toTuple (Point vec) =
    Vec.toTuple vec
