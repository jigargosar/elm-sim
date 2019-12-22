module GravitronV6.World exposing (World, init, toList, update)

import GravitronV6.Entity exposing (Entity)
import Playground exposing (..)


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


update : Computer -> World -> World
update computer (World nid entities) =
    World nid (List.map (stepEntity computer entities) entities)


stepEntity : Computer -> List Entity -> Entity -> Entity
stepEntity computer entities e =
    List.foldl (performAliveStep computer) e e.aliveSteps


performAliveStep { time, screen } step e =
    let
        ( x, y ) =
            ( wave screen.left screen.right 6 time
            , wave screen.top screen.bottom 8 time
            )
    in
    { e | x = x, y = y }
