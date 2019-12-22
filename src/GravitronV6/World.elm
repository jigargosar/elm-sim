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
        ( Acc genEntities, updatedEntities ) =
            List.foldl (stepEntity computer oldEntities) ( emptyAcc, [] ) oldEntities
    in
    List.foldl addNew (World nid updatedEntities) genEntities
        |> reverseWorld


type Acc
    = Acc (List Entity)


emptyAcc : Acc
emptyAcc =
    Acc []


accumulate : Entity -> Acc -> Acc
accumulate e (Acc list) =
    e :: list |> Acc


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity : Computer -> List Entity -> Entity -> ( Acc, List Entity ) -> ( Acc, List Entity )
stepEntity computer allEntities e ( gen, accUpdated ) =
    List.foldl (performAliveStep computer allEntities) ( gen, [], e ) e.aliveSteps
        |> collectAliveSteps accUpdated


collectAliveSteps : List Entity -> ( Acc, List AliveStep, Entity ) -> ( Acc, List Entity )
collectAliveSteps accUpdated ( accGen, steps, e ) =
    ( accGen, { e | aliveSteps = steps, x = e.x + e.vx, y = e.y + e.vy } :: accUpdated )


performAliveStep : Computer -> List Entity -> AliveStep -> ( Acc, List AliveStep, Entity ) -> ( Acc, List AliveStep, Entity )
performAliveStep computer allEntities step ( gen, stepAcc, e ) =
    performAliveStepHelp computer allEntities step e
        |> (\( genF, newStep, newE ) -> ( genF gen, newStep :: stepAcc, newE ))


performAliveStepHelp : Computer -> List Entity -> AliveStep -> Entity -> ( Acc -> Acc, AliveStep, Entity )
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
                                accumulate (Circ.shoot e to rec.speed rec.template)

                            Nothing ->
                                identity

                    else
                        identity
            in
            ( genF, newStep, e )


findNamed : a -> List { b | name : a } -> Maybe { b | name : a }
findNamed name =
    List.Extra.find (propEq .name name)
