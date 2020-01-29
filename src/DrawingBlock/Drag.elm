module DrawingBlock.Drag exposing (Model, Msg(..), data, getA, init, initDown, subscriptions, update)

import Browser.Events as BE
import Json.Decode as JD exposing (Decoder)


type Model a b
    = Down a b
    | Drag a b
    | Up


type Msg a b
    = OnDrag a b
    | OnClick a b
    | OnDrop a b


init : Model a b
init =
    Up


initDown : a -> b -> Model a b
initDown =
    Down


data : Model a b -> Maybe b
data model =
    case model of
        Up ->
            Nothing

        Down a b ->
            Just b

        Drag a b ->
            Just b


getA : Model a b -> Maybe a
getA model =
    case model of
        Up ->
            Nothing

        Down a b ->
            Just a

        Drag a b ->
            Just a


update : Msg a b -> Model a b -> Model a b
update message model =
    case message of
        OnDrag a b ->
            Drag a b

        OnClick _ _ ->
            Up

        OnDrop _ _ ->
            Up


subscriptions : Decoder b -> Model a b -> Sub (Msg a b)
subscriptions bDecoder state =
    let
        decoder x =
            JD.map x bDecoder
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
