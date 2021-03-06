module GravitronV6.World exposing (World, init, stepAll, toList)

import Frame2d
import GravitronV6.Circ as Circ
import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, FireModel, NewEntity, Phase(..))
import GravitronV6.Geom as Geom
import List.Extra
import Pixels
import Playground exposing (..)
import Point2d
import PointFree exposing (cons)
import Quantity exposing (Quantity)
import Rectangle2d
import Stack exposing (Stack)


type World
    = World Number (List Entity)


init : List NewEntity -> World
init =
    List.foldr addNew (World 1 [])


addNew : NewEntity -> World -> World
addNew (Entity.New e) (World nid list) =
    World (nid + 1) (cons { e | id = nid } list)


toList : World -> List Entity
toList (World _ list) =
    list


stepAll : (List Entity -> ( acc, List NewEntity )) -> Computer -> World -> ( acc, World )
stepAll afterUpdate computer (World nid oldEntities) =
    let
        emptyStacks : ( Stack NewEntity, Stack Updated )
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
        |> applyAfterUpdateHook afterUpdate
        |> Tuple.mapSecond reverseWorld


applyAfterUpdateHook : (List Entity -> ( acc, List NewEntity )) -> World -> ( acc, World )
applyAfterUpdateHook func ((World _ list) as w) =
    let
        ( acc, newList ) =
            func list
    in
    ( acc, List.foldl addNew w newList )


reverseWorld : World -> World
reverseWorld (World nid list) =
    List.reverse list |> World nid


stepEntity :
    Computer
    -> List Entity
    -> Entity
    -> ( Stack NewEntity, Stack Updated )
    -> ( Stack NewEntity, Stack Updated )
stepEntity computer allEntities entity ( newStack, updatedStack ) =
    let
        pushOnUpdated =
            Updated >> Stack.pushOn updatedStack
    in
    case entity.phase of
        SpawningPhase sm ->
            ( newStack
            , if sm.elapsed >= sm.duration then
                pushOnUpdated { entity | phase = ReadyPhase }

              else
                pushOnUpdated { entity | phase = SpawningPhase { sm | elapsed = sm.elapsed + 1 } }
            )

        ReadyPhase ->
            performAliveSteps computer allEntities newStack entity
                |> Tuple.mapSecond
                    ((\e ->
                        if e.currentHP <= 0 then
                            { e | phase = DyingPhase { elapsed = 0, duration = 60 } }

                        else
                            updateAliveSteps e
                     )
                        >> pushOnUpdated
                    )

        DyingPhase dm ->
            ( newStack
            , if dm.elapsed >= dm.duration then
                updatedStack

              else
                pushOnUpdated { entity | phase = DyingPhase { dm | elapsed = dm.elapsed + 1 } }
            )


performAliveSteps : Computer -> List Entity -> Stack NewEntity -> Entity -> ( Stack NewEntity, Entity )
performAliveSteps computer allEntities stackOfNewEntities entity =
    entity.aliveSteps
        |> List.foldl (performAliveStep computer allEntities) ( stackOfNewEntities, entity )
        |> Tuple.mapSecond Entity.moveByVelocity


performAliveStep : Computer -> List Entity -> AliveStep -> ( Stack NewEntity, Entity ) -> ( Stack NewEntity, Entity )
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
                        |> List.map Entity.new
            in
            ( Stack.pushAll firedEntityList newStack, entity )

        DieOnCollisionWith names ->
            ( newStack
            , if isCollidingWithAnyOf names allEntities entity then
                Entity.kill entity

              else
                entity
            )

        ReceiveCollisionDamageFrom names ->
            let
                hits : Float
                hits =
                    getCollisionCount names allEntities entity
                        |> toFloat
            in
            ( newStack
            , Entity.takeDamage hits entity
            )

        Wanderer ->
            let
                ss =
                    scaleScreenBy 0.8 computer.screen

                newE =
                    (if isOutsideSS then
                        Entity.gravitateTo allEntities "Player" entity

                     else
                        entity
                    )
                        |> Geom.bounceVel 1 ss

                isOutsideSS =
                    entity.x <= ss.left || entity.x >= ss.right || entity.y >= ss.top || entity.y <= ss.bottom
            in
            ( newStack, newE )


scaleScreenBy : Float -> { a | width : Float, height : Float } -> Screen
scaleScreenBy s screen =
    toScreen (screen.width * s) (screen.height * s)


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }


getCollisionCount : List String -> List Entity -> Entity -> Int
getCollisionCount names list e =
    let
        check o =
            checkCollisionHelp names o e
    in
    if e.phase == ReadyPhase then
        List.Extra.count check list

    else
        0


isCollidingWithAnyOf : List String -> List Entity -> Entity -> Bool
isCollidingWithAnyOf names list e =
    let
        check o =
            checkCollisionHelp names o e
    in
    e.phase == ReadyPhase && List.any check list


checkCollisionHelp : List String -> Entity -> Entity -> Bool
checkCollisionHelp names o e =
    (o.phase == ReadyPhase)
        && List.member o.name names
        && Geom.ccc e.x e.y e.r o.x o.y o.r
        && (e.id /= o.id)


updateAliveSteps : Entity -> Entity
updateAliveSteps entity =
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
    Entity.withAliveSteps (List.map func entity.aliveSteps) entity


type Updated
    = Updated Entity
