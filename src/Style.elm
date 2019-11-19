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


borderTop : List String -> Attribute msg
borderTop =
    String.join " " >> A.style "border-top"


borderLeft : List String -> Attribute msg
borderLeft =
    String.join " " >> A.style "border-left"


borderBottom : List String -> Attribute msg
borderBottom =
    String.join " " >> A.style "border-bottom"


borderRight : List String -> Attribute msg
borderRight =
    String.join " " >> A.style "border-right"


outline : List String -> Attribute msg
outline =
    String.join " " >> A.style "outline"


noShrink : Attribute msg
noShrink =
    A.style "flex-shrink" "0"


transform : List String -> Attribute msg
transform =
    String.join " " >> A.style "transform"
