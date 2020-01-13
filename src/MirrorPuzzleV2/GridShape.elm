module MirrorPuzzleV2.GridShape exposing (GridShape, init, move, moveCell, posToScreen, rect)

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import Playground exposing (..)
import PointFree exposing (flip)


type GridShape a
    = GS Number (Grid a)


init : Number -> Grid a -> GridShape a
init =
    GS


move : GridShape a -> Shape -> Shape
move (GS cz grid) =
    let
        ( w, h ) =
            Grid.viewDimensions cz grid
    in
    mv ( (cz - w) / 2, (cz - h) / 2 )


moveCell : ( Int, Int ) -> GridShape a -> Shape -> Shape
moveCell pos =
    flip posToScreen pos >> mv


mv ( x, y ) =
    Playground.move x y


posToScreen : GridShape a -> Pos -> ( Number, Number )
posToScreen (GS cz _) ( x, y ) =
    ( toFloat x * cz, toFloat y * cz )


rect : Color -> GridShape a -> Shape
rect color (GS cz _) =
    rectangle color cz cz


fill shape ((GS _ grid) as gs) =
    Grid.positions grid
        |> List.map (\pos -> moveCell pos gs shape)
        |> group
