module IO exposing (canvas, getBrowserWH, group, onBrowserWH, scale2, text, transform)

import Browser.Dom as BD
import Browser.Events as BE
import Html as H
import Number2 as NT exposing (Float2)
import PointFree exposing (mapEach)
import String exposing (fromFloat)
import String2 as ST
import Svg as S
import Svg.Attributes as SA
import Task


whFromRecord : { a | width : b, height : c } -> ( b, c )
whFromRecord r =
    ( r.width, r.height )


getBrowserWH : Task.Task x ( Float, Float )
getBrowserWH =
    BD.getViewport |> Task.map (.scene >> whFromRecord)


onBrowserWH : (NT.Float2 -> a) -> Sub a
onBrowserWH func =
    BE.onResize (\w h -> func (NT.toFloat ( w, h )))


canvas : Float2 -> List (S.Svg msg) -> H.Html msg
canvas wh =
    let
        ( w, h ) =
            mapEach fromFloat wh

        ( x, y ) =
            NT.scale -0.5 wh |> mapEach fromFloat
    in
    S.svg
        [ SA.viewBox (x ++ " " ++ y ++ " " ++ w ++ " " ++ h)
        , SA.style
            """
                position:fixed;
                top:0;
                left0;
                width:100%;
                height:100%;
            """
        ]


scale2 : ( Float, Float ) -> String
scale2 sxy =
    let
        ( x, y ) =
            ST.fromFloat sxy
    in
    "scale(" ++ x ++ "," ++ y ++ ")"


text : String -> List (S.Attribute msg) -> S.Svg msg
text words attributes =
    S.text_ (textAttributes attributes) [ S.text words ]


group : List (S.Attribute msg) -> List (S.Svg msg) -> S.Svg msg
group =
    S.g


transform : List String -> S.Attribute msg
transform =
    String.join " " >> SA.transform


textAttributes list =
    SA.textAnchor "middle"
        :: SA.dominantBaseline "central"
        :: list
