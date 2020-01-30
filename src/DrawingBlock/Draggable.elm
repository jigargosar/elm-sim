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


type InternalState
    = InternalState EventType Points


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
            { message = msg (Just (InternalState MouseDown (Points e e e)))
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
deltaHelp (InternalState _ { current, prev }) =
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

                Just (InternalState type_ points) ->
                    ( InternalState MouseDrag { points | prev = points.current, current = event }
                        |> Just
                    , Move
                    )

        OnUp _ ->
            case maybeState of
                Nothing ->
                    ( model, Up Drop )

                Just (InternalState type_ _) ->
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
                (\(InternalState type_ points) ->
                    let
                        newPoints current =
                            { start = points.start
                            , prev = points.current
                            , current = current
                            }
                    in
                    [ BE.onMouseMove (decoder OnMove)
                    , BE.onMouseMove
                        (IO.pageXYDecoder
                            |> JD.map
                                (\current ->
                                    OnDrag (Just (InternalState MouseDrag (newPoints current)))
                                        (newPoints current)
                                )
                        )
                    , BE.onMouseUp
                        (IO.pageXYDecoder
                            |> JD.map
                                (\current ->
                                    OnEnd Nothing
                                        (newPoints current)
                                        (case type_ of
                                            MouseDown ->
                                                Click

                                            MouseDrag ->
                                                Drop
                                        )
                                )
                        )
                    ]
                )
                maybeState
    in
    Sub.batch subs
