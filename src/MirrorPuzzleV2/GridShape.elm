module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , applyCellTransform
    , applyGridTransform
    , cellWidth
    , fill
    , fromCellSize
    , gridCordinatesToCellPos
    , render
    , transformCellPos
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import NumberTuple as NT
import Playground exposing (..)
import PointFree exposing (flip, mapEach, scaleBoth, toFloat2)


type GridShape a
    = GS ( Number, Number ) (Grid a)


fromCellSize : Number -> Grid a -> GridShape a
fromCellSize cz =
    GS ( cz, cz )


cellWidth =
    cellDimensions >> Tuple.first


cellDimensions : GridShape a -> ( Number, Number )
cellDimensions (GS cellD _) =
    cellD


toGrid : GridShape a -> Grid a
toGrid (GS _ grid) =
    grid


applyGridTransform : GridShape a -> Shape -> Shape
applyGridTransform (GS ( cw, ch ) grid) =
    let
        ( gw, gh ) =
            Grid.dimensions grid |> mapEach toFloat
    in
    mv ( (cw - (gw * cw)) / 2, (ch - (gh * ch)) / 2 )


applyCellTransform : Pos -> GridShape a -> Shape -> Shape
applyCellTransform pos =
    flip transformCellPos pos >> mv


mv ( x, y ) =
    Playground.move x y


transformCellPos : GridShape a -> Pos -> ( Number, Number )
transformCellPos (GS ( cw, ch ) _) =
    toFloat2 >> scaleBoth ( cw, ch )


gridCordinatesToCellPos : GridShape a -> ( Number, Number ) -> Pos
gridCordinatesToCellPos (GS ( cw, ch ) grid) cord =
    let
        gridD =
            Grid.dimensions grid |> NT.toFloat

        cellD =
            ( cw, ch )

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
        |> List.map (\pos -> applyCellTransform pos gs shape)
        |> group
        |> applyGridTransform gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func gs =
    toGrid gs
        |> Grid.map (\pos cell -> applyCellTransform pos gs (func pos cell))
        |> Grid.values
        |> group
        |> applyGridTransform gs
