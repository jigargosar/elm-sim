module GravitronV2.Vector2 exposing
    ( RecXY
    , Vec
    , dirOppX
    , dirOppY
    , dirX
    , dirY
    , fromRec
    , mapX
    , opp
    , plus
    , scale
    , setX
    , toRec
    , vec
    , vec0
    , vec1
    )


type Vec
    = Vec Float Float


vec : Float -> Float -> Vec
vec =
    Vec


vec0 : Vec
vec0 =
    vec 0 0


vec1 : Vec
vec1 =
    vec 1 1


dirX : Vec
dirX =
    vec 1 0


dirY : Vec
dirY =
    vec 0 1


dirOppX : Vec
dirOppX =
    vec -1 0


dirOppY : Vec
dirOppY =
    vec 0 -1


opp : Vec -> Vec
opp =
    scale -1


scale : Float -> Vec -> Vec
scale s (Vec x y) =
    vec (x * s) (y * s)


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
plus (Vec x1 y1) (Vec x2 y2) =
    vec (x1 + x2) (y1 + y2)
