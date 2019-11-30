module GravitronV2.Render exposing (Color, black, canvas, fillRectLeftTop, fullScreenCanvas)

import Color
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types


type Color
    = Color Color.Color


black : Color
black =
    Color Color.black


fullScreenCanvas : Float -> Float -> Float -> Float -> Color -> List (Svg msg) -> Html msg
fullScreenCanvas x y width height (Color fillColor) children =
    TypedSvg.svg
        [ Html.Attributes.style "position" "fixed"
        , TypedSvg.Attributes.viewBox x y width height
        , InPx.width width
        , InPx.height height
        ]
        (TypedSvg.rect
            [ InPx.x x
            , InPx.y y
            , InPx.width width
            , InPx.height height
            , TypedSvg.Attributes.fill (TypedSvg.Types.Fill fillColor)
            ]
            []
            :: children
        )
