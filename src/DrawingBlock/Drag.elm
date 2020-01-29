module DrawingBlock.Drag exposing
    ( Event
    , Model
    , Msg(..)
    , eventDecoder
    , getA
    , init
    , initDown
    , subscriptions
    , update
    )

import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import Number2 exposing (Float2)


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
    = OnDrag a Event
    | OnClick a Event
    | OnDrop a Event


init : Model a
init =
    Up


initDown : a -> Event -> Model a
initDown =
    Down


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
        OnDrag a b ->
            Drag a b

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
