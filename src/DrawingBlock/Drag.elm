module DrawingBlock.Drag exposing
    ( Event
    , Model
    , Msg(..)
    , delta
    , eventDecoder
    , getA
    , init
    , initDown
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


initDown : a -> Event -> Model a
initDown a event =
    update (OnDown a event) init


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


getA : Model a -> Maybe a
getA model =
    case model of
        Up ->
            Nothing

        Down a _ ->
            Just a

        Drag a _ ->
            Just a


update : Msg a -> Model a -> Model a
update message _ =
    case message of
        OnDown a event ->
            Down a event

        OnDrag a event ->
            Drag a event

        OnClick _ _ ->
            Up

        OnDrop _ _ ->
            Up


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
