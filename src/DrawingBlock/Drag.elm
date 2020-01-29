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


type Model a
    = Down a Event
    | Drag a Event
    | Up


type Msg a
    = OnDown a Event
    | OnDrag a Event
    | OnClick a Event
    | OnDrop a Event


init : Model a
init =
    Up


onDown : a -> VirtualDom.Attribute (Msg a)
onDown a =
    VirtualDom.on "mousedown"
        (VirtualDom.Normal
            (JD.map (OnDown a) eventDecoder)
        )


delta : Model a -> Model b -> ( Float, Float )
delta oldModel newModel =
    Maybe.map2 Tuple.pair (getEvent newModel) (getEvent oldModel)
        |> Maybe.map (mapEach .pageXY >> uncurry N2.sub)
        |> Maybe.withDefault ( 0, 0 )


getEvent : Model a -> Maybe Event
getEvent model =
    case model of
        Down _ e ->
            Just e

        Drag _ e ->
            Just e

        Up ->
            Nothing


type OutMsg a
    = Start a
    | Move Float2
    | Click
    | End


update : Msg a -> Model a -> ( Model a, OutMsg a )
update message model =
    case message of
        OnDown a event ->
            ( Down a event, Start a )

        OnDrag a event ->
            let
                newModel =
                    Drag a event
            in
            ( newModel, Move (delta model newModel) )

        OnClick _ _ ->
            ( Up, Click )

        OnDrop _ _ ->
            ( Up, End )


subscriptions : Model a -> Sub (Msg a)
subscriptions state =
    let
        decoder x =
            JD.map x eventDecoder
    in
    case state of
        Up ->
            Sub.none

        Down a _ ->
            [ BE.onMouseUp (decoder (OnClick a))
            , BE.onMouseMove (decoder (OnDrag a))
            ]
                |> Sub.batch

        Drag a _ ->
            [ BE.onMouseUp (decoder (OnDrop a))
            , BE.onMouseMove (decoder (OnDrag a))
            ]
                |> Sub.batch
