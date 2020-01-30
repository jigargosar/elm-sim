module DrawingBlock.Drag exposing
    ( DragModel
    , Event
    , Msg
    , OutMsg(..)
    , delta
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


type alias DragModel =
    Maybe State


type alias State =
    { start : Event
    , prev : Event
    , current : Event
    , type_ : EventType
    }


type EventType
    = MouseDown
    | MouseDrag


type Msg
    = OnMove Event
    | OnUp Event


intial : DragModel
intial =
    Nothing


fromEvent msg e =
    { message = msg (Just (State e e e MouseDown))
    , preventDefault = True
    , stopPropagation = True
    }


onDown : (DragModel -> msg) -> VirtualDom.Attribute msg
onDown msg =
    VirtualDom.on "mousedown"
        (VirtualDom.Custom
            (JD.map (fromEvent msg) eventDecoder)
        )


delta : DragModel -> ( Float, Float )
delta maybeState =
    Maybe.map deltaHelp maybeState |> Maybe.withDefault ( 0, 0 )


deltaHelp : State -> ( Float, Float )
deltaHelp { current, prev } =
    ( current, prev ) |> (mapEach .pageXY >> uncurry N2.sub)


type OutMsg
    = Move
    | Click
    | End


update : Msg -> DragModel -> ( DragModel, OutMsg )
update message (maybeState as model) =
    case message of
        OnMove event ->
            case maybeState of
                Nothing ->
                    ( model, End )

                Just state ->
                    ( { state | prev = state.current, current = event, type_ = MouseDrag }
                        |> Just
                    , Move
                    )

        OnUp _ ->
            case maybeState of
                Nothing ->
                    ( model, End )

                Just { type_ } ->
                    ( Nothing
                    , case type_ of
                        MouseDown ->
                            Click

                        MouseDrag ->
                            End
                    )


subscriptions : DragModel -> Sub Msg
subscriptions maybeState =
    let
        decoder x =
            JD.map x eventDecoder

        subs =
            case maybeState of
                Nothing ->
                    []

                _ ->
                    [ BE.onMouseUp (decoder OnUp)
                    , BE.onMouseMove (decoder OnMove)
                    ]
    in
    Sub.batch subs
