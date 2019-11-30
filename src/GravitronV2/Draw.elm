module GravitronV2.Draw exposing (..)

import Color exposing (Color)
import Svg
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types


rectTopLeft : Float -> Float -> Float -> Float -> Color -> List (Svg.Attribute msg) -> Svg.Svg msg
rectTopLeft top left width height fillColor arr =
    TypedSvg.rect
        ([ TypedSvg.Attributes.InPx.x top
         , TypedSvg.Attributes.InPx.y left
         , TypedSvg.Attributes.InPx.width width
         , TypedSvg.Attributes.InPx.height height
         , TypedSvg.Attributes.fill (TypedSvg.Types.Fill fillColor)
         ]
            ++ arr
        )
        []
