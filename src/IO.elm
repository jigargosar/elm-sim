module IO exposing (MouseEvent, buttonDecoder, canvas, getBrowserSize, group, mouseEventDecoder, movementXYDecoder, onBrowserResize, pageXYDecoder, scale, scale2, stopAllOn, styleNode, text, textGroup, timeStampDecoder, transform, tspan)

import Browser.Dom as BD
import Browser.Events as BE
import Html as H
import Json.Decode as JD exposing (Decoder)
import Number2 as NT exposing (Float2)
import PointFree exposing (mapEach)
import String exposing (fromFloat)
import String2 as ST
import Svg as S
import Svg.Attributes as SA
import Task
import VirtualDom


whFromRecord : { a | width : b, height : c } -> ( b, c )
whFromRecord r =
    ( r.width, r.height )


getBrowserSize : Task.Task x ( Float, Float )
getBrowserSize =
    BD.getViewport |> Task.map (.scene >> whFromRecord)


onBrowserResize : (NT.Float2 -> a) -> Sub a
onBrowserResize func =
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


styleNode : String -> VirtualDom.Node msg
styleNode =
    VirtualDom.text >> List.singleton >> VirtualDom.node "style" []


scale : Float -> String
scale =
    NT.singleton >> scale2


text : String -> List (S.Attribute msg) -> S.Svg msg
text words attributes =
    textGroup attributes [ S.text words ]


tspan : String.String -> List (S.Attribute msg) -> S.Svg msg
tspan words attributes =
    S.tspan attributes [ S.text words ]


textGroup : List (S.Attribute msg) -> List (S.Svg msg) -> S.Svg msg
textGroup attributes =
    S.text_ (textAttributes attributes)


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


pageXYDecoder : JD.Decoder ( Float, Float )
pageXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


movementXYDecoder : JD.Decoder ( Float, Float )
movementXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "movementX" JD.float)
        (JD.field "movementY" JD.float)


buttonDecoder : Decoder Int
buttonDecoder =
    JD.field "button" JD.int


timeStampDecoder : JD.Decoder Float
timeStampDecoder =
    JD.field "timeStamp" JD.float


type alias MouseEvent =
    { pageXY : Float2
    , movementXY : Float2
    , button : Int
    }


mouseEventDecoder : Decoder MouseEvent
mouseEventDecoder =
    JD.map3 MouseEvent
        pageXYDecoder
        movementXYDecoder
        buttonDecoder


type alias CustomHandler msg =
    { message : msg, preventDefault : Bool, stopPropagation : Bool }


stopAll : a -> CustomHandler a
stopAll msg =
    CustomHandler msg True True


stopAllHandler : Decoder a -> VirtualDom.Handler a
stopAllHandler decoder =
    VirtualDom.Custom (JD.map stopAll decoder)


stopAllOn : String -> Decoder a -> VirtualDom.Attribute a
stopAllOn eventName decoder =
    VirtualDom.on eventName (stopAllHandler decoder)
