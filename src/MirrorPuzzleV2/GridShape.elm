module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , cellWidth
    , fill
    , foldl
    , fromCellSize
    , get
    , insert
    , move
    , moveCell
    , posFromScreen
    , posToScreen
    , render
    , setCellSize
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import Playground exposing (..)
import PointFree exposing (flip, mapEach)


type GridShape a
    = GS Number Number (Grid a)


fromCellSize : Number -> Grid a -> GridShape a
fromCellSize cz =
    GS cz cz


setCellSize : Number -> GridShape a -> GridShape a
setCellSize cz (GS _ _ grid) =
    fromCellSize cz grid


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
posToScreen (GS cw ch _) ( x, y ) =
    ( toFloat x * cw, toFloat y * ch )


posFromScreen : GridShape a -> ( Number, Number ) -> Pos
posFromScreen (GS cw ch grid) ( sx, sy ) =
    let
        ( gw, gh ) =
            Grid.dimensions grid |> mapEach toFloat

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


get : Pos -> GridShape a -> Maybe a
get pos (GS _ _ grid) =
    Grid.get pos grid


mapGrid : (Grid b -> Grid a) -> GridShape b -> GridShape a
mapGrid func (GS cw ch grid) =
    func grid |> GS cw ch


insert : Pos -> a -> GridShape a -> GridShape a
insert pos a =
    mapGrid (Grid.insert pos a)


foldl : (Pos -> a -> b -> b) -> b -> GridShape a -> b
foldl a b =
    toGrid >> Grid.foldl a b


toGrid : GridShape a -> Grid a
toGrid (GS _ _ grid) =
    grid
