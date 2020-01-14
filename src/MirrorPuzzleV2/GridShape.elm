module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , fill
    , init
    , move
    , moveCell
    , posToScreen
    , render
    )

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


moveCell : Pos -> GridShape a -> Shape -> Shape
moveCell pos =
    flip posToScreen pos >> mv


mv ( x, y ) =
    Playground.move x y


posToScreen : GridShape a -> Pos -> ( Number, Number )
posToScreen (GS cz _) ( x, y ) =
    ( toFloat x * cz, toFloat y * cz )


fill : Shape -> GridShape a -> Shape
fill shape ((GS _ grid) as gs) =
    Grid.positions grid
        |> List.map (\pos -> moveCell pos gs shape)
        |> group
        |> move gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func ((GS _ grid) as gs) =
    Grid.map (\pos cell -> moveCell pos gs (func pos cell)) grid
        |> Grid.values
        |> group
        |> move gs
