module GravitronV5.Step exposing (..)

import GravitronV5.EntityConfig as EC
import GravitronV5.Names exposing (Name)
import Playground exposing (Number)


type Step
    = Move EC.Move
    | Fire { every : Int, named : Name, towards : Name, speed : Number } Int


perform :
    { a
        | onMove : EC.Move -> ( b, EC.Move )
        , onFire : { every : Int, named : Name, towards : Name, speed : Number } -> Int -> ( b, Int )
    }
    -> Step
    -> ( b, Step )
perform { onMove, onFire } step =
    let
        ( response, newStep ) =
            case step of
                Move m ->
                    onMove m |> Tuple.mapSecond Move

                Fire conf elapsed ->
                    onFire conf elapsed
                        |> Tuple.mapSecond (Fire conf)
    in
    ( response, newStep )
