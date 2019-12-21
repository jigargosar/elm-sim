module GravitronV5.Main2 exposing (main)

import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), Step(..))
import GravitronV5.World as World exposing (World, WorldConfig)
import Playground exposing (..)


type Name
    = Player
    | Turret
    | Bullet


playerConfig : EntityConfig Name
playerConfig =
    EC.named Player
        (\rec ->
            { rec
                | r = 20
                , color = green
                , step = [ Move RandomWalker ]
            }
        )


turretConfig : EntityConfig Name
turretConfig =
    EC.named Turret
        (\rec ->
            { rec
                | r = 25
                , color = red
                , step = [ Fire Bullet ]
            }
        )


configOf : Name -> EntityConfig Name
configOf name =
    case name of
        Player ->
            playerConfig

        Turret ->
            turretConfig

        _ ->
            EC.named name identity


worldConfig : WorldConfig Name
worldConfig =
    { singletonNames = [ Player ]
    , configOf = configOf
    }


withXY : ( Number, Number ) -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
withXY ( x, y ) e =
    { e | x = x, y = y }


initialEntities : List (EntityConfig Name)
initialEntities =
    let
        turrets =
            [ ( -150, 150 ) ]
                |> List.map
                    (\pos ->
                        configOf Turret
                            |> EC.map (withXY pos)
                    )
    in
    configOf Player :: turrets


initialMemory : World Name
initialMemory =
    World.init worldConfig initialEntities


updateMemory : Computer -> World Name -> World Name
updateMemory computer world =
    World.update worldConfig computer world


viewMemory : Computer -> World Name -> List Shape
viewMemory computer world =
    World.toList worldConfig world
        |> List.map viewEntity


viewEntity : World.Entity Name -> Shape
viewEntity entity =
    let
        { x, y, r, color } =
            entity
    in
    group [ circle color r ]
        |> move x y


main =
    game viewMemory updateMemory initialMemory
