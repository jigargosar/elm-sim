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
        ( geList, ueList ) =
            List.foldl
                (\e ( geAcc, ueAcc ) ->
                    stepEntity computer oldEntities e
                        |> (\( ge, ue ) -> ( ge :: geAcc, ue :: ueAcc ))
                )
                ( [], [] )
                oldEntities

        newEntities =
            foldBatch (\(New e) acc -> e :: acc) [] geList
    in
    List.foldl addNew (World nid ueList) newEntities
        |> reverseWorld


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity :
    Computer
    -> List Entity
    -> Entity
    -> ( BatchNew, Entity )
stepEntity computer allEntities e =
    List.foldl
        (\step ( geAcc, stepAcc, entityAcc ) ->
            performAliveStep computer allEntities step entityAcc
                |> (\( ge, updatedStep, updatedEntity ) -> ( BatchConcat [ ge, geAcc ], updatedStep :: stepAcc, updatedEntity ))
        )
        ( BatchNone, [], e )
        e.aliveSteps
        |> (\( geAcc, stepAcc, entityAcc ) ->
                ( geAcc, Entity.withAliveSteps stepAcc entityAcc )
           )
        |> Tuple.mapSecond Entity.moveByVelocity


performAliveStep :
    Computer
    -> List Entity
    -> AliveStep
    -> Entity
    -> ( BatchNew, AliveStep, Entity )
performAliveStep computer allEntities step e =
    case step of
        WalkRandomly ->
            ( BatchNone, step, Entity.performRandomWalk computer e )

        Fire rec ->
            let
                ( ge, newStep ) =
                    performFire e allEntities rec
            in
            ( ge, newStep, e )


findNamed : a -> List { b | name : a } -> Maybe { b | name : a }
findNamed name =
    List.Extra.find (propEq .name name)


type alias FireModel =
    { elapsed : Number, every : Number, toName : String, speed : Float, template : Entity }


performFire : Entity -> List Entity -> FireModel -> ( BatchNew, AliveStep )
performFire from allEntities rec =
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

        generatedEntities : BatchNew
        generatedEntities =
            if triggered then
                case findNamed rec.toName allEntities of
                    Just to ->
                        BatchOne (New (Circ.shoot from to rec.speed rec.template))

                    Nothing ->
                        BatchNone

            else
                BatchNone
    in
    ( generatedEntities, newStep )


type alias BatchNew =
    Batch New


type New
    = New Entity


type Batch a
    = BatchConcat (List (Batch a))
    | BatchOne a
    | BatchNone


foldBatch : (a -> b -> b) -> b -> List (Batch a) -> b
foldBatch func =
    let
        reducer : Batch a -> b -> b
        reducer batch acc =
            case batch of
                BatchNone ->
                    acc

                BatchConcat batches ->
                    List.foldl reducer acc batches

                BatchOne a ->
                    func a acc
    in
    List.foldl reducer
