module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig as EntityConfig exposing (EntityConfig)
import Playground exposing (Computer)


type World name
    = World Int (List (Entity name))


type alias WorldConfig name =
    { singletonNames : List name, configOf : name -> EntityConfig name }


init : WorldConfig name -> List (EntityConfig name) -> World name
init _ initialEntityConfigList =
    let
        entityList =
            initialEntityConfigList
                |> List.map fromConfig
    in
    World (List.length entityList) entityList


update : WorldConfig name -> Computer -> World name -> World name
update computer worldConfig world =
    world


type alias Entity name =
    { name : name }


fromConfig : EntityConfig name -> Entity name
fromConfig =
    let
        fromConfigRec { name } =
            { name = name }
    in
    EntityConfig.toRec >> fromConfigRec


toList : WorldConfig name -> World name -> List (Entity name)
toList worldConfig world =
    []
