module GravitronV6.World exposing (World, init, toList, update)

import GravitronV6.Circ as Circ
import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity)
import List.Extra
import Playground exposing (..)
import PointFree exposing (cons, propEq)
import Stack exposing (Stack)


type World
    = World Number (List Entity)


init : List Entity -> World
init =
    List.foldl addNew (World 1 [])


addNew : Entity -> World -> World
addNew e (World nid list) =
    World (nid + 1) (cons { e | id = nid } list)


toList : World -> List Entity
toList (World _ list) =
    list


update : Computer -> World -> World
update computer (World nid oldEntities) =
    let
        ( newStack, updatedStack ) =
            List.foldl
                (stepEntity computer oldEntities)
                ( Stack.empty, Stack.empty )
                oldEntities

        newEntities =
            Stack.map (\(New e) -> e) newStack |> Stack.toLifo
    in
    List.foldl addNew (World nid (Stack.map (\(Updated e) -> e) updatedStack |> Stack.toLifo)) newEntities
        |> reverseWorld


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity :
    Computer
    -> List Entity
    -> Entity
    -> ( Stack New, Stack Updated )
    -> ( Stack New, Stack Updated )
stepEntity computer allEntities e ( newStack, updatedStack ) =
    List.foldl
        (\step ( nStack, stepAcc, entityAcc ) ->
            performAliveStep computer allEntities step entityAcc
                |> (\( newList, updatedStep, updatedEntity ) ->
                        ( Stack.pushAll newList nStack, updatedStep :: stepAcc, updatedEntity )
                   )
        )
        ( newStack, [], e )
        e.aliveSteps
        |> (\( geAcc, stepAcc, entityAcc ) ->
                ( geAcc, Entity.withAliveSteps stepAcc entityAcc )
           )
        |> Tuple.mapSecond Entity.moveByVelocity
        |> Tuple.mapSecond (Updated >> Stack.pushOn updatedStack)


performAliveStep :
    Computer
    -> List Entity
    -> AliveStep
    -> Entity
    -> ( List New, AliveStep, Entity )
performAliveStep computer allEntities step e =
    case step of
        WalkRandomly ->
            ( [], step, Entity.performRandomWalk computer e )

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


performFire : Entity -> List Entity -> FireModel -> ( List New, AliveStep )
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

        generatedEntities =
            if triggered then
                case findNamed rec.toName allEntities of
                    Just to ->
                        [ New (Circ.shoot from to rec.speed rec.template) ]

                    Nothing ->
                        []

            else
                []
    in
    ( generatedEntities, newStep )


type New
    = New Entity


type Updated
    = Updated Entity
