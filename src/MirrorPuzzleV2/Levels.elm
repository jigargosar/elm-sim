module MirrorPuzzleV2.Levels exposing
    ( Levels
    , count
    , current
    , initial
    , next
    , prev
    )

import Cons exposing (Cons)
import MirrorPuzzleV2.PuzzleGrid as PuzzleGrid
import Pivot exposing (Pivot)
import PointFree exposing (ignoreNothing)


type alias Lvl =
    PuzzleGrid.Model


type Levels
    = Levels (Pivot Lvl)


levelCons : ( PuzzleGrid.Model, List PuzzleGrid.Model )
levelCons =
    Cons.cons """
         __,__,__,__,__,__,__,__
         __,S ,M0,__,__,__,D ,__
         __,__,__,__,__,__,__,__
         """
        [ """
          __,__,__,__,__,__,__,__
          __,S0,__,__,__,__,D ,__
          __,__,__,__,__,__,__,__
          """
        ]
        |> Cons.map PuzzleGrid.fromString
        |> Cons.uncons


initial : Levels
initial =
    let
        ( first, rest ) =
            levelCons
    in
    Levels (Pivot.fromCons first rest)


current : Levels -> Lvl
current (Levels pivot) =
    Pivot.getC pivot


prev : Levels -> ( Lvl, Levels )
prev (Levels pivot) =
    ignoreNothing (Pivot.goBy -1) pivot
        |> withCurrent


next : Levels -> ( Lvl, Levels )
next (Levels pivot) =
    ignoreNothing (Pivot.goBy 1) pivot
        |> withCurrent


count : Levels -> Int
count (Levels p) =
    Pivot.lengthA p


withCurrent : Pivot Lvl -> ( Lvl, Levels )
withCurrent pivot =
    ( Pivot.getC pivot, Levels pivot )
