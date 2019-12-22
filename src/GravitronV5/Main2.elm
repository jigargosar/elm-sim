module GravitronV5.Main2 exposing (main)

import Ease
import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), PreStep(..), Step(..))
import GravitronV5.HP as HP
import GravitronV5.Names exposing (Name(..))
import GravitronV5.World as World exposing (World, WorldConfig)
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)


playerConfig : EntityConfig
playerConfig =
    EC.named Player
        (\rec ->
            { rec
                | r = 20
                , color = green
                , steps = [ Move RandomWalker ]
            }
        )


turretConfig : EntityConfig
turretConfig =
    EC.named Turret
        (\rec ->
            { rec
                | r = 25
                , color = red
                , spawnDuration = 120
                , preSteps = [ ReceiveCollisionDamage [ Bullet ] ]
                , steps = [ Fire Bullet Player ]
                , maxHP = 1
            }
        )


bulletConfig : EntityConfig
bulletConfig =
    EC.named Bullet
        (\rec ->
            { rec
                | r = 10
                , color = black
                , preSteps = [ DieOnCollision [ Bullet, Player, Turret ] ]
                , steps = [ Move (BounceInScreen 0.5), Move (GravitateTo Player) ]
            }
        )


configOf : Name -> EntityConfig
configOf name =
    case name of
        Player ->
            playerConfig

        Turret ->
            turretConfig

        Bullet ->
            bulletConfig


afterUpdate : List World.Entity -> List EntityConfig
afterUpdate list =
    let
        turretCount =
            List.Extra.count (propEq .name Turret) list

        newTurrets =
            if turretCount == 0 then
                initialTurrets

            else
                []
    in
    newTurrets


worldConfig : WorldConfig
worldConfig =
    { configOf = configOf
    , afterUpdate = afterUpdate
    }


withXY : ( Number, Number ) -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
withXY ( x, y ) e =
    { e | x = x, y = y }


initialTurrets =
    [ ( -150, 150 ) ]
        |> List.map
            (\pos ->
                turretConfig
                    |> EC.map (withXY pos)
            )


initialEntities : List EntityConfig
initialEntities =
    configOf Player :: initialTurrets


initialMemory : World
initialMemory =
    World.init worldConfig initialEntities


updateMemory : Computer -> World -> World
updateMemory computer world =
    World.update worldConfig computer world


viewMemory : Computer -> World -> List Shape
viewMemory computer world =
    World.toList worldConfig world
        |> List.map viewEntity


viewEntity : World.Entity -> Shape
viewEntity entity =
    let
        { x, y, phase } =
            entity
    in
    toCoreShape entity |> phaseTransition phase |> move x y


toCoreShape { name, r, color, hp } =
    group
        [ circle color r
        , if name == Turret then
            words darkCharcoal (String.fromInt (HP.remaining hp))

          else
            group []
        ]


phaseTransition phase =
    case phase of
        World.ReadyForCollision ->
            identity

        World.Dying hi now ->
            let
                pro =
                    toFloat now / toFloat hi

                maxFade =
                    0.7
            in
            fade (maxFade - (pro * maxFade)) >> scale (1 + pro / 2)

        World.Spawning hi now ->
            let
                pro =
                    toFloat now / toFloat hi
            in
            scale (Ease.outElastic pro)


main =
    game viewMemory updateMemory initialMemory
