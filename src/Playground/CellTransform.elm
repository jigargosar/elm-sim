module Playground.CellTransform exposing
    ( CellTransform
    , fromPos
    , init
    , toPos
    , width
    , xyToPos
    )

import Number2 as NT
import Playground.Grid as Grid exposing (Grid, Pos)
import PointFree exposing (flip)


type alias Transform =
    { scale : NT.Float2, translate : NT.Float2 }


type CellTransform
    = CT Model


type alias Model =
    { cellD : NT.Float2
    , cellT : NT.Float2
    }


unwrap : CellTransform -> Model
unwrap (CT model) =
    model


map : (Model -> Model) -> CellTransform -> CellTransform
map func =
    unwrap >> func >> CT


init : ( Float, Float ) -> Grid a -> CellTransform
init cellD grid =
    CT
        { cellD = cellD
        , cellT = Grid.dimensions grid |> NT.toFloat |> NT.mul cellD |> NT.sub cellD |> NT.scale 0.5
        }
        |> Debug.log "ct"


width : CellTransform -> Float
width (CT m) =
    m.cellD |> Tuple.first


fromPos : CellTransform -> Pos -> NT.Float2
fromPos (CT { cellD, cellT }) pos =
    pos |> NT.toFloat |> NT.mul cellD |> NT.add cellT


toPos : CellTransform -> NT.Float2 -> Pos
toPos (CT { cellT, cellD }) cord =
    cord
        |> flip NT.sub cellT
        |> flip NT.div cellD
        |> NT.round


xyToPos : CellTransform -> { a | x : Float, y : Float } -> Pos
xyToPos ct xy =
    toPos ct (NT.fromXY xy)
