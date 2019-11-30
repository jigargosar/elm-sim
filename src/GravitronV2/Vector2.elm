module GravitronV2.Vector2 exposing
    ( RecXY
    , Vec
    , fromRec
    , mapX
    , plus
    , setX
    , toRec
    , vec
    , vec0
    )


type Vec
    = Vec Float Float


vec : Float -> Float -> Vec
vec =
    Vec


vec0 : Vec
vec0 =
    vec 0 0


fromRec : { a | x : Float, y : Float } -> Vec
fromRec { x, y } =
    vec x y


type alias RecXY =
    { x : Float, y : Float }


toRec : Vec -> RecXY
toRec (Vec x y) =
    { x = x, y = y }


mapX : (Float -> Float) -> Vec -> Vec
mapX func (Vec x y) =
    Vec (func x) y


setX : Float -> Vec -> Vec
setX =
    mapX << always


plus : Vec -> Vec -> Vec
plus v1 v2 =
    v1
