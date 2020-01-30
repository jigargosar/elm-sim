module DrawingBlock.Draggable exposing
    ( Draggable
    , Msg
    , OutMsg(..)
    , PageXY
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
import Maybe.Extra
import Number2 as N2 exposing (Float2)
import VirtualDom


type alias PageXY =
    Float2


type alias Draggable =
    Maybe InternalState


type alias InternalState =
    { start : PageXY
    , prev : PageXY
    , current : PageXY
    , type_ : EventType
    }


type EventType
    = MouseDown
    | MouseDrag


type alias Points =
    { start : PageXY
    , prev : PageXY
    , current : PageXY
    }


type Msg
    = OnMove PageXY
    | OnUp PageXY
    | OnEnd Draggable Points End
    | OnDrag Draggable Points


intial : Draggable
intial =
    Nothing


fromEvent msg =
    JD.map
        (\e ->
            { message = msg (Just (InternalState e e e MouseDown))
            , preventDefault = True
            , stopPropagation = True
            }
        )
        IO.pageXYDecoder


onDown : (Draggable -> msg) -> VirtualDom.Attribute msg
onDown msg =
    VirtualDom.on "mousedown"
        (VirtualDom.Custom (fromEvent msg))


delta : Draggable -> ( Float, Float )
delta maybeState =
    Maybe.map deltaHelp maybeState |> Maybe.withDefault ( 0, 0 )


deltaHelp : InternalState -> ( Float, Float )
deltaHelp { current, prev } =
    ( current, prev ) |> uncurry N2.sub


type OutMsg
    = Move
    | Up End


type End
    = Click
    | Drop


update : Msg -> Draggable -> ( Draggable, OutMsg )
update message (maybeState as model) =
    case message of
        OnMove event ->
            case maybeState of
                Nothing ->
                    ( model, Up Drop )

                Just state ->
                    ( { state | prev = state.current, current = event, type_ = MouseDrag }
                        |> Just
                    , Move
                    )

        OnUp _ ->
            case maybeState of
                Nothing ->
                    ( model, Up Drop )

                Just { type_ } ->
                    ( Nothing
                    , case type_ of
                        MouseDown ->
                            Up Click

                        MouseDrag ->
                            Up Drop
                    )

        OnEnd draggable _ up ->
            ( draggable, Up up )

        OnDrag draggable _ ->
            ( draggable, Move )


subscriptions : Draggable -> Sub Msg
subscriptions maybeState =
    let
        decoder x =
            JD.map x IO.pageXYDecoder

        subs =
            Maybe.Extra.unwrap []
                (\state ->
                    let
                        newPoints current =
                            { start = state.start
                            , prev = state.current
                            , current = current
                            }
                    in
                    [ BE.onMouseMove (decoder OnMove)
                    , case state.type_ of
                        MouseDown ->
                            BE.onMouseUp
                                (IO.pageXYDecoder
                                    |> JD.map
                                        (\current ->
                                            OnEnd Nothing
                                                (newPoints current)
                                                (case state.type_ of
                                                    MouseDown ->
                                                        Click

                                                    MouseDrag ->
                                                        Drop
                                                )
                                        )
                                )

                        MouseDrag ->
                            BE.onMouseUp (decoder OnUp)
                    ]
                )
                maybeState
    in
    Sub.batch subs
