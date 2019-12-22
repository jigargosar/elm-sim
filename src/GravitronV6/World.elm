module GravitronV6.World exposing (World, init, toList, update)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity)
import List.Extra
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
update computer (World nid oldEntities) =
    let
        ( newEntities, updatedEntities ) =
            List.foldl (stepEntity computer) ( [], [] ) oldEntities
                |> Tuple.mapSecond List.reverse
    in
    List.foldl addNew (World nid updatedEntities) newEntities


stepEntity : Computer -> Entity -> ( List Entity, List Entity ) -> ( List Entity, List Entity )
stepEntity computer e ( newAcc, updatedAcc ) =
    List.foldl (performAliveStep computer) ( newAcc, [], e ) e.aliveSteps
        |> setAliveSteps updatedAcc


setAliveSteps updatedAcc ( newAcc, steps, e ) =
    ( newAcc, { e | aliveSteps = steps } :: updatedAcc )


performAliveStep computer step ( newAcc, stepAcc, e ) =
    case step of
        WalkRandomly ->
            ( newAcc, step :: stepAcc, Entity.performRandomWalk computer e )

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
            ( newAcc, newStep :: stepAcc, e )
