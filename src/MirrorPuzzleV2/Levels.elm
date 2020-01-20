module MirrorPuzzleV2.Levels exposing
    ( Levels
    , count
    , current
    , fromIndex
    , goTo
    , index
    , initial
    , isLast
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


firstLevel =
    """
        __,__,__,__,__,__,__,__
        __,S ,M0,__,__,__,D ,__
        __,__,__,__,__,__,__,__
    """


otherLevels =
    [ """
    __,__,__,__,__,__,__,__
    __,D ,__,__,M6,__,S ,__
    __,__,__,__,__,__,__,__
      """
    , """
    X ,X ,X ,X ,X ,X ,X ,X ,X ,X
    X ,__,S ,__,__,__,__,__,__,X
    X ,__,__,__,M6,__,__,__,__,X
    X ,__,__,__,__,__,__,__,__,X
    X ,X ,X ,X ,X ,X ,__,__,__,X
    X ,__,__,__,__,__,__,__,__,X
    X ,__,__,__,M6,__,__,__,__,X
    X ,__,__,__,__,__,__,D ,__,X
    X ,__,__,__,__,__,__,__,__,X
      """
    , """
    X ,X ,X ,X ,X ,X ,X ,X ,X ,X
    X ,__,__,__,__,__,__,__,__,X
    X ,__,S ,__,M6,__,__,__,__,X
    X ,__,__,__,__,__,__,__,__,X
    X ,X ,X ,X ,X ,X ,X ,__,__,X
    X ,X ,X ,X ,X ,X ,X ,__,__,X
    X ,__,__,__,M2,__,__,__,__,X
    X ,__,D ,__,__,__,__,__,__,X
    X ,__,__,__,__,M0,__,__,__,X
      """
    ]


levelCons : ( PuzzleGrid.Model, List PuzzleGrid.Model )
levelCons =
    Cons.cons firstLevel
        otherLevels
        |> Cons.map PuzzleGrid.fromString
        |> Cons.uncons


initial : Levels
initial =
    let
        ( first, rest ) =
            levelCons
    in
    Levels (Pivot.fromCons first rest)
        |> goTo (List.length otherLevels)


fromIndex : Int -> Levels
fromIndex i =
    goTo i initial


current : Levels -> Lvl
current (Levels pivot) =
    Pivot.getC pivot


index : Levels -> Int
index (Levels pivot) =
    Pivot.lengthL pivot


prev : Levels -> Levels
prev (Levels pivot) =
    ignoreNothing (Pivot.goBy -1) pivot
        |> Levels


next : Levels -> Levels
next (Levels pivot) =
    ignoreNothing (Pivot.goBy 1) pivot
        |> Levels


goTo : Int -> Levels -> Levels
goTo idx (Levels pivot) =
    ignoreNothing (Pivot.goAbsolute idx) pivot
        |> Levels


isLast : Levels -> Bool
isLast (Levels p) =
    Pivot.getR p |> List.isEmpty


count : Levels -> Int
count (Levels p) =
    Pivot.lengthA p
