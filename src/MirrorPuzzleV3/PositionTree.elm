module MirrorPuzzleV3.PositionTree exposing (..)

import Graph.Tree as Tree exposing (Tree)
import Number2 exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import PointFree exposing (flip)
import Set exposing (Set)


unfold :
    { a
        | position : Int2
        , directions : List Direction8
        , getNextDirections : Direction8 -> Int2 -> Maybe (List Direction8)
    }
    -> Tree Int2
unfold config =
    let
        next : Seed -> ( Int2, List Seed )
        next seed =
            ( seed.position
            , List.filterMap (flip nextSeedInDirection seed) seed.directions
            )
    in
    Tree.unfoldTree next (initSeed config)


type alias Seed =
    { visitedPositions : Set Int2
    , position : Int2
    , directions : List Direction8
    , nextDirections : Direction8 -> Int2 -> Maybe (List Direction8)
    }


initSeed :
    { a
        | position : Int2
        , directions : List Direction8
        , getNextDirections : Direction8 -> Int2 -> Maybe (List Direction8)
    }
    -> Seed
initSeed config =
    Seed Set.empty config.position config.directions config.getNextDirections


nextSeedInDirection : Direction8 -> Seed -> Maybe Seed
nextSeedInDirection direction seed =
    let
        nextPosition =
            D.stepPos direction seed.position

        nextSeedFromDirections : List Direction8 -> Seed
        nextSeedFromDirections directions =
            { seed
                | visitedPositions = Set.insert nextPosition seed.visitedPositions
                , position = nextPosition
                , directions = directions
            }
    in
    if Set.member nextPosition seed.visitedPositions then
        Nothing

    else
        seed.nextDirections direction nextPosition
            |> Maybe.map nextSeedFromDirections
