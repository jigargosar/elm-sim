module Playground.CellTransform exposing (..)

import Number2 as NT exposing (Float2, Int2)
import PointFree exposing (toFloat2)


type alias CellTransform =
    { toView : Int2 -> Float2
    , fromView : Float2 -> Int2
    , cellSize : Float
    }


fromViewD : Float2 -> Int2 -> CellTransform
fromViewD viewD gridD =
    let
        cellSize =
            viewD |> NT.divBy (toFloat2 gridD) |> NT.apply min

        cellD =
            ( cellSize, cellSize )

        dxy =
            gridD |> NT.toFloat |> NT.mul cellD |> NT.sub cellD |> NT.scale 0.5
    in
    { cellSize = cellSize
    , toView = NT.toFloat >> NT.mul cellD >> NT.add dxy
    , fromView = NT.subBy dxy >> NT.divBy cellD >> NT.round
    }
