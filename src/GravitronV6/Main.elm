module GravitronV6.Main exposing (main)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity)
import GravitronV6.World as World exposing (World)
import Playground exposing (..)


type Name
    = Player
    | Turret
    | Bullet


name n =
    case n of
        Player ->
            "Player"

        Turret ->
            "Turret"

        Bullet ->
            "Bullet"


default : Entity
default =
    Entity.default


bulletTemplate =
    { default
        | name = name Bullet
        , r = 10
        , color = charcoal
        , vx = 1
        , vy = 1
        , aliveSteps = []
    }


init : World
init =
    [ { default | name = name Player, r = 20, color = green, aliveSteps = [ WalkRandomly ] }
    , { default
        | name = name Turret
        , r = 25
        , color = red
        , aliveSteps =
            [ Fire
                { elapsed = 0
                , didTrigger = False
                , every = 60
                , towards = name Player
                , template = bulletTemplate
                , speed = 3
                }
            ]
      }
    ]
        |> List.map World.newEntity
        |> World.init


update : Computer -> World -> World
update =
    World.update


view : Computer -> World -> List Shape
view _ =
    World.toList >> List.indexedMap viewEntity


viewEntity idx e =
    group
        [ circle e.color e.r
        , ("z-" ++ String.fromInt idx)
            |> words black
            |> moveDown (e.r + 10)
        ]
        |> move e.x e.y


main =
    game view update init
