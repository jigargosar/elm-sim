module View exposing (cell)

import Color
import Svg exposing (Svg)
import TypedSvg exposing (rect)
import TypedSvg.Attributes exposing (fill)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Types exposing (Fill(..))


cell : Float -> Int -> Int -> Bool -> Svg msg
cell cellWidthInPx gridX gridY isAlive =
    let
        xInPx =
            toFloat gridX * cellWidthInPx + 1

        yInPx =
            toFloat gridY * cellWidthInPx + 1
    in
    rect
        [ (if isAlive then
            Color.lightRed

           else
            Color.lightYellow
          )
            |> Fill
            |> fill
        , x xInPx
        , y yInPx
        , width cellWidthInPx
        , height cellWidthInPx
        ]
        []
