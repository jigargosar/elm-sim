module GravitronV6.World exposing (World, init, toList, update)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity)
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
    List.foldl (performAliveStep computer) ( [], e ) e.aliveSteps
        |> setAliveSteps


setAliveSteps ( steps, e ) =
    { e | aliveSteps = steps }


performAliveStep computer step ( steps, e ) =
    case step of
        WalkRandomly ->
            ( step :: steps, Entity.performRandomWalk computer e )

        Fire rec ->
            let
                triggered =
                    rec.elapsed >= rec.every

                newRec =
                    if triggered then
                        { rec | elapsed = 0 }

                    else
                        { rec | elapsed = rec.elapsed + 1 }

                newStep =
                    Fire newRec
            in
            ( newStep :: steps, e )
