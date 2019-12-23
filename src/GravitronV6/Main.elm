module GravitronV6.Main exposing (main)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, Phase(..))
import GravitronV6.World as World exposing (World)
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)


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


bulletTemplate : Entity
bulletTemplate =
    { default
        | name = name Bullet
        , r = 10
        , color = charcoal
        , vx = 1
        , vy = 1
        , aliveSteps =
            [ GravitateTo (name Player)
            , BounceInScreen 0.5
            , DieOnCollisionWith (names [ Player, Turret, Bullet ])
            ]
    }


initialTurret =
    { default
        | name = name Turret
        , r = 25
        , color = red
        , phase = Spawning { elapsed = 0, duration = 60 }
        , aliveSteps =
            [ Fire
                { elapsed = 0
                , didTrigger = False
                , every = 60
                , towards = name Player
                , template = bulletTemplate
                , speed = 3
                }
            , ReceiveCollisionDamageFrom (names [ Bullet ])
            ]
    }


init : World
init =
    [ { default | name = name Player, r = 20, color = green, aliveSteps = [ WalkRandomly ] }
    , initialTurret
    ]
        |> List.map World.newEntity
        |> World.init


update : Computer -> World -> World
update =
    World.update afterUpdateHook


afterUpdateHook =
    List.Extra.count (propEq .name (name Turret))
        >> (\tc ->
                if tc == 0 then
                    [ World.newEntity initialTurret ]

                else
                    []
           )


view : Computer -> World -> List Shape
view _ =
    World.toList >> List.indexedMap viewEntity


viewEntity : Int -> Entity -> Shape
viewEntity idx e =
    toShape idx e
        |> applyPhaseTransform e.phase
        |> move e.x e.y


applyPhaseTransform : Phase -> Shape -> Shape
applyPhaseTransform phase shape =
    case phase of
        Spawning sm ->
            let
                progress =
                    sm.elapsed / sm.duration
            in
            shape
                |> fade (progress + 0.1)
                |> scale (progress + 0.1)

        Alive ->
            shape

        Dying dm ->
            let
                progress =
                    dm.elapsed / dm.duration
            in
            shape
                |> fade (1 - progress)
                |> scale (1 + progress / 2)


toShape : Int -> Entity -> Shape
toShape idx e =
    group
        [ circle e.color e.r
        , ("z-" ++ String.fromInt idx)
            |> words black
            |> moveDown (e.r + 10)
        , if name Turret == e.name then
            group
                [ String.fromInt (round e.currentHP)
                    |> words white
                ]

          else
            group []
        ]


main =
    game view update init
