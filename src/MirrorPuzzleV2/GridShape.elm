module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , applyCellTransformToPos
    , applyCellTransformToShape
    , applyGridTransformToShape
    , cellWidth
    , fill
    , fromCellSize
    , render
    , transformGridCordinatesToPos
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import Playground exposing (..)
import PointFree exposing (flip, mapEach, scaleBoth, toFloat2)


type GridShape a
    = GS Number Number (Grid a)


fromCellSize : Number -> Grid a -> GridShape a
fromCellSize cz =
    GS cz cz


cellWidth : GridShape a -> Number
cellWidth (GS cw _ _) =
    cw


applyGridTransformToShape : GridShape a -> Shape -> Shape
applyGridTransformToShape (GS cw ch grid) =
    let
        ( gw, gh ) =
            Grid.dimensions grid |> mapEach toFloat
    in
    mv ( (cw - (gw * cw)) / 2, (ch - (gh * ch)) / 2 )


applyCellTransformToShape : Pos -> GridShape a -> Shape -> Shape
applyCellTransformToShape pos =
    flip applyCellTransformToPos pos >> mv


mv ( x, y ) =
    Playground.move x y


applyCellTransformToPos : GridShape a -> Pos -> ( Number, Number )
applyCellTransformToPos (GS cw ch _) =
    toFloat2 >> scaleBoth ( cw, ch )


transformGridCordinatesToPos : GridShape a -> ( Number, Number ) -> Pos
transformGridCordinatesToPos (GS cw ch grid) ( sx, sy ) =
    let
        ( gw, gh ) =
            Grid.dimensions grid |> toFloat2

        ( dx, dy ) =
            ( (cw - (gw * cw)) / 2, (ch - (gh * ch)) / 2 )
    in
    ( (sx - dx) / cw, (sy - dy) / ch ) |> mapEach round


fill : Shape -> GridShape a -> Shape
fill shape ((GS _ _ grid) as gs) =
    Grid.positions grid
        |> List.map (\pos -> applyCellTransformToShape pos gs shape)
        |> group
        |> applyGridTransformToShape gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func ((GS _ _ grid) as gs) =
    Grid.map (\pos cell -> applyCellTransformToShape pos gs (func pos cell)) grid
        |> Grid.values
        |> group
        |> applyGridTransformToShape gs
