module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , cellWidth
    , fill
    , fromCellSize
    , move
    , moveCell
    , posFromScreen
    , posToScreen
    , render
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
cellWidth (GS cw _ grid) =
    cw


move : GridShape a -> Shape -> Shape
move (GS cw ch grid) =
    let
        ( gw, gh ) =
            Grid.dimensions grid |> mapEach toFloat
    in
    mv ( (cw - (gw * cw)) / 2, (ch - (gh * ch)) / 2 )


moveCell : Pos -> GridShape a -> Shape -> Shape
moveCell pos =
    flip posToScreen pos >> mv


mv ( x, y ) =
    Playground.move x y


posToScreen : GridShape a -> Pos -> ( Number, Number )
posToScreen (GS cw ch _) =
    toFloat2 >> scaleBoth ( cw, ch )


posFromScreen : GridShape a -> ( Number, Number ) -> Pos
posFromScreen (GS cw ch grid) ( sx, sy ) =
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
        |> List.map (\pos -> moveCell pos gs shape)
        |> group
        |> move gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func ((GS _ _ grid) as gs) =
    Grid.map (\pos cell -> moveCell pos gs (func pos cell)) grid
        |> Grid.values
        |> group
        |> move gs
