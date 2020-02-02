module GravitronV3.Screen exposing (Screen, get, initial, onResize, toSvg)

import Browser.Dom as Dom
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes as H
import PointFree exposing (mapEach)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
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


toScreen : ( Float, Float ) -> Screen
toScreen ( width, height ) =
    { width = width
    , height = height
    , top = -height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = height / 2
    }


initial : Screen
initial =
    toScreen ( 600, 600 )


toSvg : Screen -> List (Svg msg) -> Html msg
toSvg s =
    svg
        [ T.viewBox s.left s.top s.width s.height
        , width "100%"
        , height "100%"
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%"
        ]


fromResizeEvent : Int -> Int -> Screen
fromResizeEvent w h =
    ( w, h ) |> mapEach toFloat |> toScreen


onResize : (Screen -> msg) -> Sub msg
onResize msg =
    E.onResize fromResizeEvent |> Sub.map msg


fromViewport : Dom.Viewport -> Screen
fromViewport { viewport } =
    toScreen ( viewport.width, viewport.height )


get : Task.Task Never Screen
get =
    Dom.getViewport |> Task.map fromViewport
