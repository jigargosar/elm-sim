module GravitronV3.Main exposing (..)

import Basics.Extra exposing (flip)
import Browser
import Browser.Dom as Dom
import Html.Attributes as H
import Playground exposing (Screen)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


toViewBox : Float -> Float -> Float -> Float -> Svg.Attribute msg
toViewBox x y w h =
    [ x, y, w, h ] |> List.map String.fromFloat |> String.join " " |> viewBox


appendWith =
    flip (++)


toPx : (String -> a) -> Float -> a
toPx attr value =
    attr (value |> String.fromFloat |> appendWith "px")



-- Main


pairedTo : b -> a -> ( a, b )
pairedTo =
    flip Tuple.pair


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach func =
    Tuple.mapBoth func func


type Msg
    = GotViewport Dom.Viewport


toScreen : ( Float, Float ) -> Screen
toScreen ( width, height ) =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }


view =
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


type alias Model =
    { screen : Screen }


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { screen = toScreen ( 600, 600 ) }
                , Task.perform GotViewport Dom.getViewport
                )
        , update = \_ -> pairedTo Cmd.none
        , subscriptions = \_ -> Sub.none
        , view = \_ -> view
        }
