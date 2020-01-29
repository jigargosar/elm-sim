module DrawingBlock.Drag exposing
    ( Drag
    , Event
    , Msg
    , OutMsg(..)
    , intial
    , onDown
    , subscriptions
    , update
    )

import Basics.Extra exposing (uncurry)
import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import Number2 as N2 exposing (Float2)
import PointFree exposing (mapEach)
import VirtualDom


type alias Event =
    { pageXY : Float2 }


eventDecoder : Decoder Event
eventDecoder =
    JD.map Event IO.pageXYDecoder


type Drag
    = Down Event
    | Drag Event
    | Up


type Msg
    = OnMove Event
    | OnUp Event


intial : Drag
intial =
    Up


onDown : (Drag -> msg) -> VirtualDom.Attribute msg
onDown msg =
    VirtualDom.on "mousedown"
        (VirtualDom.Normal
            (JD.map (Down >> msg) eventDecoder)
        )


delta : Drag -> Drag -> ( Float, Float )
delta oldModel newModel =
    Maybe.map2 Tuple.pair (getEvent newModel) (getEvent oldModel)
        |> Maybe.map (mapEach .pageXY >> uncurry N2.sub)
        |> Maybe.withDefault ( 0, 0 )


getEvent : Drag -> Maybe Event
getEvent model =
    case model of
        Down e ->
            Just e

        Drag e ->
            Just e

        Up ->
            Nothing


type OutMsg
    = Move Float2
    | Click
    | End


update : Msg -> Drag -> ( Drag, OutMsg )
update message model =
    case message of
        OnMove event ->
            case model of
                Up ->
                    ( Up, End )

                Down _ ->
                    let
                        newModel =
                            Drag event
                    in
                    ( newModel, Move (delta model newModel) )

                Drag _ ->
                    let
                        newModel =
                            Drag event
                    in
                    ( newModel, Move (delta model newModel) )

        OnUp _ ->
            case model of
                Up ->
                    ( Up, End )

                Down _ ->
                    ( Up, Click )

                Drag _ ->
                    ( Up, End )


subscriptions : Drag -> Sub Msg
subscriptions state =
    let
        decoder x =
            JD.map x eventDecoder

        subs =
            case state of
                Up ->
                    []

                _ ->
                    [ BE.onMouseUp (decoder OnUp)
                    , BE.onMouseMove (decoder OnMove)
                    ]
    in
    Sub.batch subs
