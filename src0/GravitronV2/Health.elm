module GravitronV2.Health exposing
    ( Health
    , dec
    , init
    , isAlive
    , isDead
    , kill
    , normalized
    )

import PointFree exposing (clamp0)


type Health
    = Health Float Float


init : Float -> Health
init maxHealth =
    let
        absMaxHealth =
            abs maxHealth
    in
    Health absMaxHealth absMaxHealth


isAlive : Health -> Bool
isAlive (Health _ health) =
    health > 0


isDead : Health -> Bool
isDead =
    isAlive >> not


kill : Health -> Health
kill =
    mapCurrentHealth (always 0)


normalized : Health -> Float
normalized (Health maxHealth health) =
    clamp0 maxHealth health / maxHealth


mapCurrentHealth : (Float -> Float) -> Health -> Health
mapCurrentHealth func (Health maxHealth health) =
    func health
        |> clamp0 maxHealth
        |> Health maxHealth


dec : Health -> Health
dec =
    mapCurrentHealth PointFree.dec
