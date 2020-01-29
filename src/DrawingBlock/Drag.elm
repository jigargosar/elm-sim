module DrawingBlock.Drag exposing
    ( Event
    , Model
    , Msg
    , OutMsg(..)
    , delta
    , init
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


type Model
    = Down Event
    | Drag Event
    | Up


type Msg a
    = OnDown a Event
    | OnDrag Event
    | OnClick Event
    | OnDrop Event


init : Model
init =
    Up


onDown : a -> VirtualDom.Attribute (Msg a)
onDown a =
    VirtualDom.on "mousedown"
        (VirtualDom.Normal
            (JD.map (OnDown a) eventDecoder)
        )


delta : Model -> Model -> ( Float, Float )
delta oldModel newModel =
    Maybe.map2 Tuple.pair (getEvent newModel) (getEvent oldModel)
        |> Maybe.map (mapEach .pageXY >> uncurry N2.sub)
        |> Maybe.withDefault ( 0, 0 )


getEvent : Model -> Maybe Event
getEvent model =
    case model of
        Down e ->
            Just e

        Drag e ->
            Just e

        Up ->
            Nothing


type OutMsg a
    = Start a
    | Move Float2
    | Click
    | End


update : Msg a -> Model -> ( Model, OutMsg a )
update message model =
    case message of
        OnDown a event ->
            ( Down event, Start a )

        OnDrag event ->
            let
                newModel =
                    Drag event
            in
            ( newModel, Move (delta model newModel) )

        OnClick _ ->
            ( Up, Click )

        OnDrop _ ->
            ( Up, End )


subscriptions : Model -> Sub (Msg a)
subscriptions state =
    let
        decoder x =
            JD.map x eventDecoder
    in
    case state of
        Up ->
            Sub.none

        Down _ ->
            [ BE.onMouseUp (decoder OnClick)
            , BE.onMouseMove (decoder OnDrag)
            ]
                |> Sub.batch

        Drag _ ->
            [ BE.onMouseUp (decoder OnDrop)
            , BE.onMouseMove (decoder OnDrag)
            ]
                |> Sub.batch
