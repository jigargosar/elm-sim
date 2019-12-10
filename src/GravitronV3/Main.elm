module GravitronV3.Main exposing (..)

import Basics.Extra exposing (flip)
import Html.Attributes as H
import Svg exposing (..)
import Svg.Attributes exposing (..)


toViewBox : Float -> Float -> Float -> Float -> Svg.Attribute msg
toViewBox x y w h =
    [ x, y, w, h ] |> List.map String.fromFloat |> String.join " " |> viewBox


appendWith =
    flip (++)


toPx : (String -> a) -> Float -> a
toPx attr value =
    attr (value |> String.fromFloat |> appendWith "px")


main =
    svg
        [ toViewBox 0 0 300 300
        , width "100%"
        , height "100%"
        , H.style "position" "fixed"
        , H.style "top" "0"
        , H.style "left" "0"
        , width "100%"
        , height "100%"
        ]
        [ Svg.rect
            [ toPx x 0
            , toPx y 0
            , width "100%"
            , height "100%"
            , fill "#000"
            ]
            []
        ]
