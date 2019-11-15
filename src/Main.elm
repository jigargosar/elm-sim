module Main exposing (main)

import Browser
import Html exposing (text)


type alias Screen =
    { w : Float, h : Float }


main =
    Browser.element


view _ =
    text "hi"
