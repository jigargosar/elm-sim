module MirrorPuzzleV2.GridShape exposing (GridShape, init, move, moveCell)

import MirrorPuzzleV2.Grid as Grid exposing (Grid)
import Playground exposing (..)


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
    Playground.move ((cz - w) / 2) ((cz - h) / 2)


moveCell : ( Int, Int ) -> GridShape a -> Shape -> Shape
moveCell ( x, y ) (GS cz _) =
    Playground.move (toFloat x * cz) (toFloat y * cz)
