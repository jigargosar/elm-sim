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
import NumberTuple as NT
import Playground exposing (..)
import PointFree exposing (flip, mapEach, toFloat2)
import Transform exposing (Transform)


type GridShape a
    = GridShape (Model a)


type alias Model a =
    { cellD : NT.Float
    , cellT : List Transform
    , gridT : List Transform
    , grid : Grid a
    }


unwrap : GridShape a -> Model a
unwrap (GridShape model) =
    model


map func =
    unwrap >> func >> GridShape


fromCellSize : Number -> Grid a -> GridShape a
fromCellSize cz =
    Model ( cz, cz ) [ Transform.scale2 ( cz, cz ) ] [ Transform.scale2 ( cz, cz ) ]
        >> GridShape


cellWidth =
    cellDimensions >> Tuple.first


cellDimensions : GridShape a -> ( Number, Number )
cellDimensions =
    unwrap >> .cellD


toGrid : GridShape a -> Grid a
toGrid =
    unwrap >> .grid


applyGridTransform : GridShape a -> Shape -> Shape
applyGridTransform gs =
    let
        grid =
            toGrid gs

        ( cw, ch ) =
            cellDimensions gs

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
transformCellPos gs =
    toFloat2 >> NT.mul (cellDimensions gs)


gridCordinatesToCellPos : GridShape a -> ( Number, Number ) -> Pos
gridCordinatesToCellPos gs cord =
    let
        gridD =
            Grid.dimensions (toGrid gs) |> NT.toFloat

        cellD =
            cellDimensions gs

        leftBottom =
            NT.mul gridD cellD
                |> NT.sub cellD
                |> NT.scale 0.5
    in
    NT.sub cord leftBottom
        |> flip NT.div cellD
        |> NT.round


fill : Shape -> GridShape a -> Shape
fill shape gs =
    toGrid gs
        |> Grid.positions
        |> List.map (\pos -> applyCellTransform pos gs shape)
        |> group
        |> applyGridTransform gs


render : (Pos -> a -> Shape) -> GridShape a -> Shape
render func gs =
    toGrid gs
        |> Grid.map (\pos cell -> applyCellTransform pos gs (func pos cell))
        |> Grid.values
        |> group
        |> applyGridTransform gs
