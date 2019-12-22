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
        ( Acc newEntities, Acc updatedEntities ) =
            List.foldl (stepEntity computer oldEntities)
                ( emptyAcc NewEntity, emptyAcc UpdatedEntity )
                oldEntities
    in
    List.foldl addNew (World nid updatedEntities) newEntities
        |> reverseWorld


type NewEntity
    = NewEntity


type UpdatedEntity
    = UpdatedEntity


type Acc tag
    = Acc (List Entity)


emptyAcc : tag -> Acc tag
emptyAcc _ =
    Acc []


accumulate : Entity -> Acc tag -> Acc tag
accumulate e (Acc list) =
    e :: list |> Acc


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity :
    Computer
    -> List Entity
    -> Entity
    -> ( Acc NewEntity, Acc UpdatedEntity )
    -> ( Acc NewEntity, Acc UpdatedEntity )
stepEntity computer allEntities e ( accNew, accUpdated ) =
    List.foldl (performAliveStep computer allEntities) ( accNew, [], e ) e.aliveSteps
        |> collectAliveStepsAndMoveByVelocity
        |> Tuple.mapSecond ((|>) accUpdated)


collectAliveStepsAndMoveByVelocity :
    ( Acc NewEntity, List AliveStep, Entity )
    -> ( Acc NewEntity, Acc UpdatedEntity -> Acc UpdatedEntity )
collectAliveStepsAndMoveByVelocity ( accNew, steps, e ) =
    ( accNew, e |> Entity.withAliveSteps steps |> Entity.moveByVelocity |> accumulate )


performAliveStep :
    Computer
    -> List Entity
    -> AliveStep
    -> ( Acc NewEntity, List AliveStep, Entity )
    -> ( Acc NewEntity, List AliveStep, Entity )
performAliveStep computer allEntities step ( accNew, stepAcc, e ) =
    performAliveStepHelp computer allEntities step e
        |> (\( accNewF, newStep, newE ) -> ( accNewF accNew, newStep :: stepAcc, newE ))


performAliveStepHelp :
    Computer
    -> List Entity
    -> AliveStep
    -> Entity
    -> ( Acc NewEntity -> Acc NewEntity, AliveStep, Entity )
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

                accNewF =
                    if triggered then
                        case findNamed rec.toName allEntities of
                            Just to ->
                                accumulate (Circ.shoot e to rec.speed rec.template)

                            Nothing ->
                                identity

                    else
                        identity
            in
            ( accNewF, newStep, e )


findNamed : a -> List { b | name : a } -> Maybe { b | name : a }
findNamed name =
    List.Extra.find (propEq .name name)
