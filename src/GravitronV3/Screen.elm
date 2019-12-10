module GravitronV3.Screen exposing (Screen, fromViewport, initial)

import Browser.Dom as Dom


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
