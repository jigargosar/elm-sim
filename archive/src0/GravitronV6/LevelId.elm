module GravitronV6.LevelId exposing (..)

import GravitronV6.Entity exposing (Entity)
import List.Extra


type alias LevelId =
    ( Int, Int )


type alias MinorLevel =
    List Entity


type alias MajorLevel =
    List MinorLevel


next : List MajorLevel -> LevelId -> LevelId
next levels ( a, b ) =
    if b + 1 >= 5 then
        ( modBy (List.length levels) a + 1, 0 )

    else
        ( a, b + 1 )


getMinorLevel : List MajorLevel -> LevelId -> Maybe MinorLevel
getMinorLevel levels ( major, minor ) =
    let
        majorMax =
            List.length levels

        minorMax =
            5
    in
    List.Extra.getAt (modBy majorMax major) levels
        |> Maybe.andThen (List.Extra.getAt (modBy minorMax minor))
