module GravitronV6.Main exposing (main)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity)
import GravitronV6.World as World exposing (World)
import Playground exposing (..)


default : Entity
default =
    Entity.default


init : World
init =
    World.init
        [ { default | r = 20, color = green, aliveSteps = [ WalkRandomly ] }
        , { default | r = 25, color = red }
        ]


update : Computer -> World -> World
update =
    World.update


view : Computer -> World -> List Shape
view _ =
    World.toList >> List.map viewEntity


viewEntity e =
    circle e.color e.r
        |> move e.x e.y


main =
    game view update init
