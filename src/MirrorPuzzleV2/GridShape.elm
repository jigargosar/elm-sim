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
import PointFree exposing (flip, mapEach)


type GridShape a
    = GS Number Number (Grid a)


init : Number -> Number -> Grid a -> GridShape a
init =
    GS


move : GridShape a -> Shape -> Shape
move (GS cw ch grid) =
    let
        ( gw, gh ) =
            Grid.dimensions grid |> mapEach toFloat
    in
    mv ( (cw - gw) / 2, (ch - gh) / 2 )


moveCell : Pos -> GridShape a -> Shape -> Shape
moveCell pos =
    flip posToScreen pos >> mv


mv ( x, y ) =
    Playground.move x y


posToScreen : GridShape a -> Pos -> ( Number, Number )
posToScreen (GS cw ch _) ( x, y ) =
    ( toFloat x * cw, toFloat y * ch )


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
