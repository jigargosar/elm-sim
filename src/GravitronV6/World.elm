module GravitronV6.World exposing (World, init, toList)

import GravitronV6.Entity exposing (Entity)
import Playground exposing (Number)


type World
    = World Number (List Entity)


init : List Entity -> World
init =
    List.foldl addNew (World 1 [])


addNew : Entity -> World -> World
addNew e (World nid list) =
    World (nid + 1) ({ e | id = nid } :: list)


toList : World -> List Entity
toList (World _ list) =
    list
