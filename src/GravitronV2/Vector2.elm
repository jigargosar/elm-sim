module GravitronV2.Vector2 exposing
    ( RecXY
    , Vec
    , add
    , dirOppX
    , dirOppY
    , dirX
    , dirY
    , fromRec
    , getX
    , getY
    , integrate
    , map2
    , mapEach
    , mapX
    , minus
    , multiply
    , opp
    , plus
    , setX
    , springForceFrom
    , subtract
    , toRec
    , toTuple
    , vec
    , vec0
    , vec1
    , vecFrom
    )


type Vec
    = Vec Float Float


getX : Vec -> Float
getX (Vec x _) =
    x


getY : Vec -> Float
getY (Vec _ y) =
    y


toTuple : Vec -> ( Float, Float )
toTuple =
    apply Tuple.pair


map2 : (Float -> Float -> Float) -> Vec -> Vec -> Vec
map2 func (Vec xa ya) (Vec xb yb) =
    vec (func xa xb) (func ya yb)


mapEach : (Float -> Float) -> Vec -> Vec
mapEach func (Vec x y) =
    vec (func x) (func y)


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
    multiply -1


multiply : Float -> Vec -> Vec
multiply s =
    mapEach (\n -> n * s)


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


integrate : Vec -> Vec -> Vec
integrate =
    map2 (+)


add : Vec -> Vec -> Vec
add =
    integrate


plus : Vec -> Vec -> Vec
plus =
    integrate


subtract : Vec -> Vec -> Vec
subtract =
    map2 (-)


vecFrom : Vec -> Vec -> Vec
vecFrom =
    subtract


minus : Vec -> Vec -> Vec
minus =
    subtract


springForceFrom : Vec -> Vec -> Float -> Vec
springForceFrom a b k =
    vecFrom a b |> multiply k


apply : (Float -> Float -> a) -> Vec -> a
apply func (Vec x y) =
    func x y
