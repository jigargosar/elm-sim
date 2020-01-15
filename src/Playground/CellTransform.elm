module Playground.CellTransform exposing
    ( CellTransform
    , fromPos
    , square
    , toPos
    , width
    )

import NumberTuple as NT
import Playground.Grid as Grid exposing (Grid, Pos)
import Transform as T exposing (Transform)


type CellTransform
    = CT Model


type alias Model =
    { cellD : NT.Float
    , gridD : NT.Float
    , cellT : List Transform
    }


unwrap : CellTransform -> Model
unwrap (CT model) =
    model


map : (Model -> Model) -> CellTransform -> CellTransform
map func =
    unwrap >> func >> CT


square : Float -> Grid a -> CellTransform
square cz grid =
    let
        cellD =
            ( cz, cz )

        gridD =
            Grid.dimensions grid |> NT.toFloat |> NT.mul cellD
    in
    CT
        { cellD = cellD
        , cellT = [ T.scale2 cellD, T.translate (NT.sub cellD gridD |> NT.scale 0.5) ]
        , gridD = gridD
        }


width : CellTransform -> Float
width (CT m) =
    m.cellD |> Tuple.first


toPos : CellTransform -> Pos -> NT.Float
toPos (CT gs) pos =
    T.transformI gs.cellT pos


fromPos : CellTransform -> NT.Float -> Pos
fromPos gs cord =
    let
        (CT { cellT }) =
            gs
    in
    cord |> T.inverseRound cellT
