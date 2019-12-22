module GravitronV5.Main2 exposing (main)

import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), PreStep(..), Step(..))
import GravitronV5.HP as HP
import GravitronV5.Names exposing (Name(..))
import GravitronV5.World as World exposing (World, WorldConfig)
import Playground exposing (..)


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
                , preSteps = [ ReceiveCollisionDamage [ Bullet ] ]
                , steps = [ Fire Bullet Player ]
                , maxHP = 10
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


worldConfig : WorldConfig
worldConfig =
    { configOf = configOf
    }


withXY : ( Number, Number ) -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
withXY ( x, y ) e =
    { e | x = x, y = y }


initialEntities : List EntityConfig
initialEntities =
    let
        turrets =
            [ ( -150, 150 ) ]
                |> List.map
                    (\pos ->
                        turretConfig
                            |> EC.map (withXY pos)
                    )
    in
    configOf Player :: turrets


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
        { name, x, y, r, color, hp, phase } =
            entity

        toCoreShape =
            group
                [ circle color r
                , if name == Turret then
                    words darkCharcoal (String.fromInt (HP.remaining hp))

                  else
                    group []
                ]
    in
    case phase of
        World.ReadyForCollision ->
            toCoreShape |> move x y

        World.Dying hi now ->
            let
                pro =
                    toFloat now / toFloat hi

                maxFade =
                    0.7
            in
            toCoreShape |> fade (maxFade - (pro * maxFade)) |> scale (1 + pro / 2) |> move x y


main =
    game viewMemory updateMemory initialMemory
