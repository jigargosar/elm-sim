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
            generatedToEntityList geList
    in
    List.foldl addNew (World nid ueList) newEntities
        |> reverseWorld


generatedToEntityList : List GeneratedEntity -> List Entity
generatedToEntityList =
    let
        reducer ge eAcc =
            case ge of
                GeneratedNone ->
                    eAcc

                GeneratedSingle e ->
                    e :: eAcc

                BatchGenerated list ->
                    List.foldl reducer eAcc list
    in
    List.foldl reducer []


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity :
    Computer
    -> List Entity
    -> Entity
    -> ( GeneratedEntity, Entity )
stepEntity computer allEntities e =
    List.foldl
        (\step ( geAcc, stepAcc, entityAcc ) ->
            performAliveStep computer allEntities step entityAcc
                |> (\( ge, updatedStep, updatedEntity ) -> ( ge :: geAcc, updatedStep :: stepAcc, updatedEntity ))
        )
        ( [], [], e )
        e.aliveSteps
        |> (\( geAcc, stepAcc, entityAcc ) ->
                ( BatchGenerated (List.reverse geAcc), Entity.withAliveSteps stepAcc entityAcc )
           )
        |> Tuple.mapSecond Entity.moveByVelocity


performAliveStep :
    Computer
    -> List Entity
    -> AliveStep
    -> Entity
    -> ( GeneratedEntity, AliveStep, Entity )
performAliveStep computer allEntities step e =
    case step of
        WalkRandomly ->
            ( GeneratedNone, step, Entity.performRandomWalk computer e )

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


performFire : Entity -> List Entity -> FireModel -> ( GeneratedEntity, AliveStep )
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

        generatedEntities : GeneratedEntity
        generatedEntities =
            if triggered then
                case findNamed rec.toName allEntities of
                    Just to ->
                        GeneratedSingle (Circ.shoot from to rec.speed rec.template)

                    Nothing ->
                        GeneratedNone

            else
                GeneratedNone
    in
    ( generatedEntities, newStep )


type GeneratedEntity
    = GeneratedNone
    | GeneratedSingle Entity
    | BatchGenerated (List GeneratedEntity)
