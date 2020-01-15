module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , cellWidth
    , fill
    , fromCellSize
    , gridCordinatesToCellPos
    , render
    , transformCell
    , transformCellPos
    , transformGrid
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import NumberTuple as NT
import Playground exposing (..)
import PointFree exposing (flip, mapEach, toFloat2)
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
        [ T.scale2 cellD ]
        [ T.translate cellD, T.translate (NT.negate gridD), T.scale 0.5 ]
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
transformGrid (GridShape { grid, cellD, cellT, gridT }) =
    T.transformOrigin gridT |> mv


transformCell : Pos -> GridShape a -> Shape -> Shape
transformCell pos =
    transformCellPos pos >> mv


mv ( x, y ) =
    Playground.move x y


transformCellPos : Pos -> GridShape a -> ( Number, Number )
transformCellPos pos (GridShape gs) =
    T.transformIntTuple pos gs.cellT


gridCordinatesToCellPos : GridShape a -> ( Number, Number ) -> Pos
gridCordinatesToCellPos gs cord =
    let
        gridD =
            Grid.dimensions (toGrid gs) |> NT.toFloat

        cellD =
            cellDimensions gs

        leftBottom =
            NT.mul gridD cellD
                |> NT.sub cellD
                |> NT.scale 0.5
    in
    NT.sub cord leftBottom
        |> flip NT.div cellD
        |> NT.round


fill : Shape -> GridShape a -> Shape
fill shape gs =
    toGrid gs
        |> Grid.positions
        |> List.map (\pos -> transformCell pos gs shape)
        |> group
        |> transformGrid gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func gs =
    toGrid gs
        |> Grid.map (\pos cell -> transformCell pos gs (func pos cell))
        |> Grid.values
        |> group
        |> transformGrid gs
