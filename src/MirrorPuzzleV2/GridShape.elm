module MirrorPuzzleV2.GridShape exposing
    ( GridShape
    , cellWidth
    , fromCellSize
    , gridCordinatesToCellPos
    , transformCellPos
    )

import MirrorPuzzleV2.Grid as Grid exposing (Grid, Pos)
import NumberTuple as NT
import Transform as T exposing (Transform)


type GridShape
    = GridShape Model


type alias Model =
    { cellD : NT.Float
    , cellT : List Transform
    }


unwrap : GridShape -> Model
unwrap (GridShape model) =
    model


map func =
    unwrap >> func >> GridShape


fromCellSize : Float -> Grid a -> GridShape
fromCellSize cz grid =
    let
        cellD =
            ( cz, cz )

        gridD =
            Grid.dimensions grid |> NT.toFloat |> NT.mul cellD
    in
    Model cellD
        [ T.scale2 cellD, T.translate (NT.sub cellD gridD |> NT.scale 0.5) ]
        |> GridShape


cellWidth =
    cellDimensions >> Tuple.first


cellDimensions : GridShape -> NT.Float
cellDimensions =
    unwrap >> .cellD


transformCellPos : GridShape -> Pos -> NT.Float
transformCellPos (GridShape gs) pos =
    T.transformI gs.cellT pos


gridCordinatesToCellPos : GridShape -> NT.Float -> Pos
gridCordinatesToCellPos gs cord =
    let
        (GridShape { cellT }) =
            gs
    in
    cord |> T.inverseRound cellT
