module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig as EntityConfig exposing (EntityConfig)
import Playground exposing (Color, Computer, Number)


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
    { name : name
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    }


fromConfig : EntityConfig name -> Entity name
fromConfig =
    let
        fromConfigRec : EntityConfig.Rec name -> Entity name
        fromConfigRec { name, x, y, r, vx, vy, color } =
            { name = name
            , x = 0
            , y = 0
            , r = 10
            , vx = 0
            , vy = 0
            , color = color
            }
    in
    EntityConfig.toRec >> fromConfigRec


toList : WorldConfig name -> World name -> List (Entity name)
toList worldConfig (World _ lst) =
    lst
