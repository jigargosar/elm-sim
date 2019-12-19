module GravitronV5.Main exposing (main)

import Playground exposing (..)


type Id
    = Id Int


type Tag
    = Player


type Data
    = Position Number Number
    | Radius Number
    | Velocity Number Number
    | PrimaryColour Color
    | PrimaryTag Tag
    | TriggerTimer Int Int


type Behaviour
    = BounceOffScreen Number
    | RandomWalker
    | ShootOnTriggerTimer
    | GravitateTo Tag
    | MoveByVelocity


type Actor
    = Actor Id (List Data) (List Behaviour)


type alias Mem =
    { actors : List Actor
    , nextId : Int
    }


initialPlayer id =
    Actor id
        [ Position 0 0, Radius 15, PrimaryColour green, PrimaryTag Player ]
        [ RandomWalker, BounceOffScreen 1, MoveByVelocity ]


initialMemory : Mem
initialMemory =
    { actors = [ initialPlayer (Id 0) ]
    , nextId = 100
    }


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    mem


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { actors } =
    List.filterMap viewActor actors


viewActor : Actor -> Maybe Shape
viewActor actor =
    case actor of
        Actor _ d _ ->
            getPrimaryTag d
                |> Maybe.andThen (viewPrimaryTag d)


viewPrimaryTag : List Data -> Tag -> Maybe Shape
viewPrimaryTag d tag =
    case tag of
        Player ->
            Maybe.map2 circle (getPrimaryColor d) (getRadius d)


getPrimaryTag : List Data -> Maybe Tag
getPrimaryTag =
    let
        reducer d answer =
            case ( answer, d ) of
                ( Just _, _ ) ->
                    answer

                ( Nothing, PrimaryTag tag ) ->
                    Just tag

                _ ->
                    answer
    in
    List.foldl reducer Nothing


getPrimaryColor : List Data -> Maybe Color
getPrimaryColor =
    let
        reducer d answer =
            case ( answer, d ) of
                ( Just _, _ ) ->
                    answer

                ( Nothing, PrimaryColour color ) ->
                    Just color

                _ ->
                    answer
    in
    List.foldl reducer Nothing


getRadius : List Data -> Maybe Number
getRadius =
    let
        reducer d answer =
            case ( answer, d ) of
                ( Just _, _ ) ->
                    answer

                ( Nothing, Radius value ) ->
                    Just value

                _ ->
                    answer
    in
    List.foldl reducer Nothing


main =
    Playground.game viewMemory updateMemory initialMemory
