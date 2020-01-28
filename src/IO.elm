module IO exposing (getBrowserWH, onBrowserWH)

import Browser.Dom as BD
import Browser.Events as BE
import Number2 as NT
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
