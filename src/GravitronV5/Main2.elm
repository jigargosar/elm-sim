module GravitronV5.Main2 exposing (main)

import GravitronV5.EntityConfig as EC exposing (EntityConfig)
import GravitronV5.World as World exposing (World, WorldConfig)
import Playground exposing (..)


type Name
    = Player
    | Turret
    | Bullet


configOf : Name -> EntityConfig Name
configOf name =
    EC.named name (\rec -> rec)


worldConfig : WorldConfig Name
worldConfig =
    { singletonNames = [ Player ]
    , configOf = configOf
    }


initialEntities : List (EntityConfig Name)
initialEntities =
    [ configOf Player ]


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
