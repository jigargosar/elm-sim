module Style exposing (..)

import Html exposing (Attribute)
import Html.Attributes as A


width : String -> Attribute msg
width =
    A.style "width"


height : String -> Attribute msg
height =
    A.style "height"


widthPx : Float -> Attribute msg
widthPx f =
    width (toPx f)


heightPx : Float -> Attribute msg
heightPx f =
    height (toPx f)


toPx : Float -> String
toPx f =
    String.fromFloat f ++ "px"


bgColor : String -> Attribute msg
bgColor =
    A.style "background-color"


border : List String -> Attribute msg
border =
    String.join " " >> A.style "border"
