module GravitronV6.World exposing (World, init, newEntity, toList, update)

import Basics.Extra exposing (swap)
import GravitronV6.Circ as Circ
import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, FireModel)
import List.Extra
import Playground exposing (..)
import PointFree exposing (cons, mapAccuml, propEq)
import Stack exposing (Stack)


type World
    = World Number (List Entity)


init : List New -> World
init =
    List.foldr addNew (World 1 [])


newEntity : Entity -> New
newEntity =
    New


addNew : New -> World -> World
addNew (New e) (World nid list) =
    World (nid + 1) (cons { e | id = nid } list)


toList : World -> List Entity
toList (World _ list) =
    list


update : Computer -> World -> World
update computer (World nid oldEntities) =
    let
        emptyStacks : ( Stack New, Stack Updated )
        emptyStacks =
            ( Stack.empty, Stack.empty )

        ( newStack, updatedStack ) =
            List.foldl
                (stepEntity computer oldEntities)
                emptyStacks
                oldEntities

        updatedEntities =
            Stack.map (\(Updated e) -> e) updatedStack |> Stack.toLifo
    in
    Stack.foldFifo addNew (World nid updatedEntities) newStack
        |> reverseWorld


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity : Computer -> List Entity -> Entity -> ( Stack New, Stack Updated ) -> ( Stack New, Stack Updated )
stepEntity computer allEntities e ( newStack, updatedStack ) =
    performAliveSteps computer allEntities newStack e
        |> Tuple.mapSecond (Entity.updateAliveSteps >> Updated >> Stack.pushOn updatedStack)


performAliveSteps : Computer -> List Entity -> Stack New -> Entity -> ( Stack New, Entity )
performAliveSteps computer allEntities stackOfNewEntities entity =
    entity.aliveSteps
        |> List.foldl (performAliveStep computer allEntities) ( stackOfNewEntities, entity )
        |> Tuple.mapSecond Entity.moveByVelocity


performAliveStep : Computer -> List Entity -> AliveStep -> ( Stack New, Entity ) -> ( Stack New, Entity )
performAliveStep computer allEntities step ( newStack, entity ) =
    case step of
        WalkRandomly ->
            ( newStack, Entity.performRandomWalk computer entity )

        GravitateTo towardsName ->
            ( newStack, Entity.gravitateTo allEntities towardsName entity )

        Fire fireModel ->
            let
                firedEntityList =
                    Entity.performFire entity allEntities fireModel
                        |> List.map New
            in
            ( Stack.pushAll firedEntityList newStack, entity )


type New
    = New Entity


type Updated
    = Updated Entity
