module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , cellWidth
    , fromCellSize
    , gridCordinatesToCellPos
    , render
    , transformCellPos
    , transformGrid
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import NumberTuple as NT
import Playground exposing (..)
import Transform as T exposing (Transform)


type GridShape a
    = GridShape (Model a)


type alias Model a =
    { cellD : NT.Float
    , cellT : List Transform
    , gridT : List Transform
    , grid : Grid a
    }


unwrap : GridShape a -> Model a
unwrap (GridShape model) =
    model


map func =
    unwrap >> func >> GridShape


fromCellSize : Number -> Grid a -> GridShape a
fromCellSize cz grid =
    let
        cellD =
            ( cz, cz )

        gridD =
            Grid.dimensions grid |> NT.toFloat |> NT.mul cellD
    in
    Model cellD
        [ T.scale2 cellD, T.translate (NT.sub cellD gridD |> NT.scale 0.5) ]
        []
        grid
        |> GridShape


cellWidth =
    cellDimensions >> Tuple.first


cellDimensions : GridShape a -> ( Number, Number )
cellDimensions =
    unwrap >> .cellD


toGrid : GridShape a -> Grid a
toGrid =
    unwrap >> .grid


transformGrid : GridShape a -> Shape -> Shape
transformGrid (GridShape { cellD, cellT, gridT }) =
    T.transformOrigin gridT |> mv


transformCell : GridShape a -> Pos -> Shape -> Shape
transformCell pos =
    transformCellPos pos >> mv


mv ( x, y ) =
    Playground.move x y


transformCellPos : GridShape a -> Pos -> ( Number, Number )
transformCellPos (GridShape gs) pos =
    T.transformI gs.cellT pos


gridCordinatesToCellPos : GridShape a -> ( Number, Number ) -> Pos
gridCordinatesToCellPos gs cord =
    let
        (GridShape { cellT, gridT }) =
            gs
    in
    cord |> T.inverse gridT |> T.inverseRound cellT


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func gs =
    toGrid gs
        |> Grid.map (\pos cell -> transformCell gs pos (func pos cell))
        |> Grid.values
        |> group
        |> transformGrid gs
