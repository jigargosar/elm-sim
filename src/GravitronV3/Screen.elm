module GravitronV3.Screen exposing (Screen, fromViewport, initial)

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as H
import Svg exposing (..)
import Svg.Attributes exposing (..)
import TypedSvg.Attributes as T


type alias Number =
    Float


type alias Screen =
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }


fromViewport : Dom.Viewport -> Screen
fromViewport { viewport } =
    toScreen ( viewport.width, viewport.height )


toScreen : ( Float, Float ) -> Screen
toScreen ( width, height ) =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }


initial : Screen
initial =
    toScreen ( 600, 600 )


toSvg : Screen -> List (Svg msg) -> Html msg
toSvg screen =
    svg
        [ T.viewBox 0 0 300 300
        , width "100%"
        , height "100%"
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%"
        ]
