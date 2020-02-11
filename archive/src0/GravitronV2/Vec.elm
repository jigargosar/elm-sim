module GravitronV2.Vec exposing
    ( RecXY
    , Vec
    , add
    , angle
    , apply2
    , clampMagnitude
    , fromPt
    , fromRTheta
    , fromRec
    , fromToScaled
    , gravityFrom
    , len
    , len2
    , lenFrom
    , map2
    , mapEach
    , mapMagnitude
    , scaleBy
    , springForceFrom
    , subtract
    , toTuple
    , vec
    , vec1
    , zero
    )

import Basics.Extra exposing (flip, uncurry)


type Vec
    = Vec Float Float


toTuple : Vec -> ( Float, Float )
toTuple =
    apply Tuple.pair


map2 : (Float -> Float -> Float) -> Vec -> Vec -> Vec
map2 func =
    apply2 func func


apply2 : (Float -> Float -> Float) -> (Float -> Float -> Float) -> Vec -> Vec -> Vec
apply2 funcX funcY (Vec xa ya) (Vec xb yb) =
    vec (funcX xa xb) (funcY ya yb)


mapEach : (Float -> Float) -> Vec -> Vec
mapEach func (Vec x y) =
    vec (func x) (func y)


vec : Float -> Float -> Vec
vec =
    Vec


zero : Vec
zero =
    vec 0 0


vec1 : Vec
vec1 =
    vec 1 1


scaleBy : Float -> Vec -> Vec
scaleBy s =
    mapEach (\n -> n * s)


fromRec : { a | x : Float, y : Float } -> Vec
fromRec { x, y } =
    vec x y


type alias RecXY =
    { x : Float, y : Float }


add : Vec -> Vec -> Vec
add =
    map2 (+)


subtract : Vec -> Vec -> Vec
subtract =
    map2 (flip (-))


fromPt : Vec -> Vec -> Vec
fromPt =
    subtract


springForceFrom : Vec -> Vec -> Float -> Vec
springForceFrom a b k =
    fromPt a b |> scaleBy k


apply : (Float -> Float -> a) -> Vec -> a
apply func (Vec x y) =
    func x y


lenFrom : Vec -> Vec -> Float
lenFrom a b =
    fromPt a b
        |> len


len2 : Vec -> Float
len2 (Vec x y) =
    x ^ 2 + y ^ 2


len : Vec -> Float
len =
    len2 >> sqrt


angle : Vec -> Float
angle (Vec x y) =
    atan2 y x


fromRTheta : Float -> Float -> Vec
fromRTheta r theta =
    vec ((*) r (cos theta))
        ((*) r (sin theta))


gravityFrom : Vec -> Vec -> Float -> Vec
gravityFrom from to mass =
    let
        gv =
            fromPt from to
    in
    fromRTheta (mass / len2 gv) (angle gv)


clampMagnitude : Float -> Vec -> Vec
clampMagnitude hi =
    let
        absHi =
            abs hi

        lo =
            negate absHi
    in
    mapMagnitude (clamp lo hi)


mapMagnitude : (Float -> Float) -> Vec -> Vec
mapMagnitude func =
    toTuple >> toPolar >> Tuple.mapFirst func >> fromPolar >> uncurry vec


fromToScaled : Vec -> Vec -> Float -> Vec
fromToScaled from to factor =
    fromPt from to |> scaleBy factor
