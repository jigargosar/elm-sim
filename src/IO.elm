module IO exposing (canvas, getBrowserWH, onBrowserWH)

import Browser.Dom as BD
import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Number2 as NT exposing (Float2)
import PointFree exposing (mapEach)
import String exposing (fromFloat)
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
        , SA.style """
            position:fixed;
            top:0;
            left0;
            width:100%;
            height:100%;
        """
        ]
