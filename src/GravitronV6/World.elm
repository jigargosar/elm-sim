module GravitronV6.World exposing (World, init, newEntity, toList, update)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, FireModel, PreStep(..))
import GravitronV6.Geom as Geom
import Playground exposing (..)
import PointFree exposing (cons)
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


isCollidingWithAnyOf names list e =
    let
        isC o =
            List.member o.name names
                && Geom.ccc e.x e.y e.r o.x o.y o.r
                && e.id
                /= o.id
    in
    List.any isC list


stepEntity : Computer -> List Entity -> Entity -> ( Stack New, Stack Updated ) -> ( Stack New, Stack Updated )
stepEntity computer allEntities =
    let
        pas newStack updatedStack e =
            performAliveSteps computer allEntities newStack e
                |> Tuple.mapSecond (updateAliveSteps >> Updated >> Stack.pushOn updatedStack)

        pre =
            performPreSteps allEntities

        do e ( ns, us ) =
            if Entity.isAlive e then
                pas ns us e

            else
                ( ns, us )
    in
    pre >> do


performPreSteps : List Entity -> Entity -> Entity
performPreSteps allEntities =
    let
        func pre entity =
            case pre of
                DieOnCollisionWith names ->
                    if isCollidingWithAnyOf names allEntities entity then
                        Entity.kill entity

                    else
                        entity
    in
    \entity -> List.foldl func entity entity.preSteps


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

        BounceInScreen factor ->
            ( newStack, Geom.bounceVel factor computer.screen entity )

        Fire fireModel ->
            let
                firedEntityList =
                    Entity.performFire entity allEntities fireModel
                        |> List.map New
            in
            ( Stack.pushAll firedEntityList newStack, entity )


updateAliveSteps : Entity -> Entity
updateAliveSteps entity =
    let
        func : AliveStep -> AliveStep
        func aliveStep =
            case aliveStep of
                WalkRandomly ->
                    aliveStep

                Fire rec ->
                    let
                        didTrigger =
                            rec.elapsed >= rec.every

                        newRec =
                            if didTrigger then
                                { rec | elapsed = 0 }

                            else
                                { rec | elapsed = rec.elapsed + 1 }
                    in
                    Fire { newRec | didTrigger = didTrigger }

                GravitateTo _ ->
                    aliveStep

                BounceInScreen _ ->
                    aliveStep
    in
    Entity.withAliveSteps (List.map func entity.aliveSteps) entity


type New
    = New Entity


type Updated
    = Updated Entity
