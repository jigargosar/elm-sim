module MirrorPuzzleV2.Levels exposing (Levels, current, next, prev)

import Pivot exposing (Pivot)
import PointFree exposing (ignoreNothing)


type Levels
    = Levels (Pivot String)


init : String -> List String -> Levels
init first rest =
    Levels (Pivot.fromCons first rest)


current : Levels -> String
current (Levels pivot) =
    Pivot.getC pivot


prev : Levels -> ( String, Levels )
prev (Levels pivot) =
    ignoreNothing (Pivot.goBy -1) pivot
        |> withCurrent


next : Levels -> ( String, Levels )
next (Levels pivot) =
    ignoreNothing (Pivot.goBy 1) pivot
        |> withCurrent


withCurrent : Pivot String -> ( String, Levels )
withCurrent pivot =
    ( Pivot.getC pivot, Levels pivot )
