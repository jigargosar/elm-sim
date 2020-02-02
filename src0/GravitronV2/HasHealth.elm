module GravitronV2.HasHealth exposing (HasHealth, Health, dec, init, isAlive, isDead, kill, normalized)

import GravitronV2.Health as Health exposing (Health)


type alias HasHealth a =
    { a | health : Health }


type alias Health =
    Health.Health


init : Float -> Health
init =
    Health.init


kill : HasHealth a -> HasHealth a
kill =
    mapHealth Health.kill


dec : HasHealth a -> HasHealth a
dec =
    mapHealth Health.dec


isAlive : HasHealth a -> Bool
isAlive =
    unwrap >> Health.isAlive


isDead : HasHealth a -> Bool
isDead =
    unwrap >> Health.isDead


normalized : HasHealth a -> Float
normalized =
    unwrap >> Health.normalized


unwrap : HasHealth a -> Health
unwrap =
    .health


mapHealth : (a -> a) -> { b | health : a } -> { b | health : a }
mapHealth func model =
    { model | health = func model.health }
