module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , cellWidth
    , fromCellSize
    , gridCordinatesToCellPos
    , transformCellPos
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import NumberTuple as NT
import Playground exposing (..)
import Transform as T exposing (Transform)


type GridShape a
    = GridShape (Model a)


type alias Model a =
    { cellD : NT.Float
    , cellT : List Transform
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
        [ T.scale2 cellD, T.translate (NT.sub cellD gridD |> NT.scale 0.5) ]
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


transformCell : GridShape a -> Pos -> Shape -> Shape
transformCell pos =
    transformCellPos pos >> mv


mv ( x, y ) =
    Playground.move x y


transformCellPos : GridShape a -> Pos -> ( Number, Number )
transformCellPos (GridShape gs) pos =
    T.transformI gs.cellT pos


gridCordinatesToCellPos : GridShape a -> ( Number, Number ) -> Pos
gridCordinatesToCellPos gs cord =
    let
        (GridShape { cellT }) =
            gs
    in
    cord |> T.inverseRound cellT
