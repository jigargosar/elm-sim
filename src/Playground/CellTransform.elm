module Playground.CellTransform exposing
    ( CellTransform
    , fromPos
    , init
    , toPos
    , width
    , xyToPos
    )

import NumberTuple as NT
import Playground.Grid as Grid exposing (Grid, Pos)
import TransformV2 as T exposing (Transform)


type CellTransform
    = CT Model


type alias Model =
    { cellD : NT.Float
    , gridD : NT.Float
    , cellT : Transform
    }


unwrap : CellTransform -> Model
unwrap (CT model) =
    model


map : (Model -> Model) -> CellTransform -> CellTransform
map func =
    unwrap >> func >> CT


init : ( Float, Float ) -> Grid a -> CellTransform
init cellD grid =
    let
        gridD =
            Grid.dimensions grid |> NT.toFloat |> NT.mul cellD
    in
    CT
        { cellD = cellD
        , cellT = T.identity |> T.scale2 cellD |> T.translate (NT.sub cellD gridD |> NT.scale 0.5)
        , gridD = gridD
        }


width : CellTransform -> Float
width (CT m) =
    m.cellD |> Tuple.first


fromPos : CellTransform -> Pos -> NT.Float
fromPos (CT gs) pos =
    T.transformI gs.cellT pos


toPos : CellTransform -> NT.Float -> Pos
toPos gs cord =
    let
        (CT { cellT }) =
            gs
    in
    cord |> T.inverseRound cellT


xyToPos : CellTransform -> { a | x : Float, y : Float } -> Pos
xyToPos ct xy =
    toPos ct (NT.fromXY xy)
