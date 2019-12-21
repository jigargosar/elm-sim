module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig exposing (EntityConfig)
import Playground exposing (Computer)


type World name
    = World


type alias WorldConfig name =
    { singletonNames : List name, configOf : name -> EntityConfig name }


init : WorldConfig name -> World name
init _ =
    World


update : WorldConfig name -> Computer -> World name -> World name
update computer worldConfig world =
    world


type alias Entity =
    {}


toList : WorldConfig name -> World name -> List Entity
toList worldConfig world =
    []
