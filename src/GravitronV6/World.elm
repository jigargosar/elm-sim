module GravitronV6.World exposing (World, init, newEntity, toList, update)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, FireModel, Phase(..), PreStep(..))
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


isCollidingWithAnyOf : List String -> List Entity -> Entity -> Bool
isCollidingWithAnyOf names list e =
    let
        check o =
            List.member o.name names
                && Geom.ccc e.x e.y e.r o.x o.y o.r
                && (e.id /= o.id)
    in
    e.phase == Alive && List.any check list


stepEntity : Computer -> List Entity -> Entity -> ( Stack New, Stack Updated ) -> ( Stack New, Stack Updated )
stepEntity computer allEntities entity ( newStack, updatedStack ) =
    let
        pushOnUpdated =
            Updated >> Stack.pushOn updatedStack
    in
    case entity.phase of
        Spawning sm ->
            ( newStack
            , if sm.elapsed >= sm.duration then
                pushOnUpdated { entity | phase = Alive }

              else
                pushOnUpdated { entity | phase = Spawning { sm | elapsed = sm.elapsed + 1 } }
            )

        Alive ->
            performAliveSteps computer allEntities newStack entity
                |> Tuple.mapSecond (updateAliveStepsIfStillAlive >> pushOnUpdated)

        Dying dm ->
            ( newStack
            , if dm.elapsed >= dm.duration then
                updatedStack

              else
                pushOnUpdated { entity | phase = Dying { dm | elapsed = dm.elapsed + 1 } }
            )


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

        DieOnCollisionWith names ->
            ( newStack
            , if isCollidingWithAnyOf names allEntities entity then
                Entity.kill entity

              else
                entity
            )


updateAliveStepsIfStillAlive : Entity -> Entity
updateAliveStepsIfStillAlive entity =
    let
        func : AliveStep -> AliveStep
        func aliveStep =
            case aliveStep of
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

                _ ->
                    aliveStep
    in
    if entity.phase == Alive then
        Entity.withAliveSteps (List.map func entity.aliveSteps) entity

    else
        entity


type New
    = New Entity


type Updated
    = Updated Entity
