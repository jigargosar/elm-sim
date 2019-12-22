module GravitronV6.World exposing (World, init, toList, update)

import GravitronV6.Circ as Circ
import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity)
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)


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
        ( genEntities, updatedEntities ) =
            List.foldl (stepEntity computer oldEntities) ( [], [] ) oldEntities
    in
    List.foldl addNew (World nid updatedEntities) genEntities
        |> reverseWorld


type AccGen
    = AccGen (List Entity)


collectGen : Entity -> AccGen -> AccGen
collectGen e (AccGen list) =
    e :: list |> AccGen


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity : Computer -> List Entity -> Entity -> ( List Entity, List Entity ) -> ( List Entity, List Entity )
stepEntity computer allEntities e ( accGen, accUpdated ) =
    List.foldl (performAliveStep computer allEntities) ( accGen, [], e ) e.aliveSteps
        |> collectAliveSteps accUpdated


collectAliveSteps : List Entity -> ( List Entity, List AliveStep, Entity ) -> ( List Entity, List Entity )
collectAliveSteps accUpdated ( accGen, steps, e ) =
    ( accGen, { e | aliveSteps = steps, x = e.x + e.vx, y = e.y + e.vy } :: accUpdated )


performAliveStep : Computer -> List Entity -> AliveStep -> ( List Entity, List AliveStep, Entity ) -> ( List Entity, List AliveStep, Entity )
performAliveStep computer allEntities step ( genAcc, stepAcc, e ) =
    performAliveStepHelp computer allEntities step e
        |> (\( genAccF, newStep, newE ) -> ( genAccF genAcc, newStep :: stepAcc, newE ))


performAliveStepHelp : Computer -> List Entity -> AliveStep -> Entity -> ( List Entity -> List Entity, AliveStep, Entity )
performAliveStepHelp computer allEntities step e =
    case step of
        WalkRandomly ->
            ( identity, step, Entity.performRandomWalk computer e )

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

                genF =
                    if triggered then
                        case findNamed rec.toName allEntities of
                            Just to ->
                                (::) (Circ.shoot e to rec.speed rec.template)

                            Nothing ->
                                identity

                    else
                        identity
            in
            ( genF, newStep, e )


findNamed : a -> List { b | name : a } -> Maybe { b | name : a }
findNamed name =
    List.Extra.find (propEq .name name)
