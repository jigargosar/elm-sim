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
    width (px f)


heightPx : Float -> Attribute msg
heightPx f =
    height (px f)


px : Float -> String
px f =
    String.fromFloat f ++ "px"


bgColor : String -> Attribute msg
bgColor =
    A.style "background-color"


border : List String -> Attribute msg
border =
    String.join " " >> A.style "border"


noShrink : Attribute msg
noShrink =
    A.style "flex-shrink" "0"


transform : List String -> Attribute msg
transform =
    String.join " " >> A.style "transform"
