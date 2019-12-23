module GravitronV6.Main exposing (main)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, PreStep(..))
import GravitronV6.World as World exposing (World)
import Playground exposing (..)


type Name
    = Player
    | Turret
    | Bullet


name : Name -> String
name n =
    case n of
        Player ->
            "Player"

        Turret ->
            "Turret"

        Bullet ->
            "Bullet"


names : List Name -> List String
names =
    List.map name


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
        , preSteps = []
        , aliveSteps = [ GravitateTo (name Player), BounceInScreen 0.5, DieOnCollisionWith (names [ Player, Turret ]) ]
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
