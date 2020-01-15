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


applyGridTransform : GridShape a -> Shape -> Shape
applyGridTransform (GS cw ch grid) =
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
transformCellPos (GS cw ch _) =
    toFloat2 >> scaleBoth ( cw, ch )


gridCordinatesToCellPos : GridShape a -> ( Number, Number ) -> Pos
gridCordinatesToCellPos (GS cw ch grid) ( sx, sy ) =
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
        |> List.map (\pos -> applyCellTransform pos gs shape)
        |> group
        |> applyGridTransform gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func ((GS _ _ grid) as gs) =
    Grid.map (\pos cell -> applyCellTransform pos gs (func pos cell)) grid
        |> Grid.values
        |> group
        |> applyGridTransform gs
