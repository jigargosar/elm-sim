module GravitronV2.Render exposing (Color, black, canvas, fillRectTopLeft)

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


fillRectTopLeft : Float -> Float -> Float -> Float -> Color -> Svg msg
fillRectTopLeft x y width height (Color fillColor) =
    TypedSvg.rect
        [ InPx.x x
        , InPx.y y
        , InPx.width width
        , InPx.height height
        , TypedSvg.Attributes.fill (TypedSvg.Types.Fill fillColor)
        ]
        []


canvas : Float -> Float -> Float -> Float -> List (Svg msg) -> Html msg
canvas x y width height =
    TypedSvg.svg
        [ Html.Attributes.style "position" "fixed"
        , TypedSvg.Attributes.viewBox x y width height
        , InPx.width width
        , InPx.height height
        ]
