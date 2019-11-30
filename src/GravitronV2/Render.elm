module GravitronV2.Render exposing (Color, black, fillRectTopLeft)

import Color
import Svg
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types


type Color
    = Color Color.Color


black : Color
black =
    Color Color.black


fillRectTopLeft : Float -> Float -> Float -> Float -> Color -> Svg.Svg msg
fillRectTopLeft top left width height (Color fillColor) =
    TypedSvg.rect
        [ TypedSvg.Attributes.InPx.x top
        , TypedSvg.Attributes.InPx.y left
        , TypedSvg.Attributes.InPx.width width
        , TypedSvg.Attributes.InPx.height height
        , TypedSvg.Attributes.fill (TypedSvg.Types.Fill fillColor)
        ]
        []
