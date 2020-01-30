module DrawingBlock.Draggable exposing
    ( Draggable
    , Msg(..)
    , PageXY
    , delta
    , intial
    , onDown
    , subscriptions
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
    = OnEnd Draggable Points End
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


type End
    = Click
    | Drop


subscriptions : (Draggable -> Msg -> msg) -> Draggable -> Sub msg
subscriptions updateDrag maybeState =
    let
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
                    [ BE.onMouseMove
                        (IO.pageXYDecoder
                            |> JD.map
                                (\current ->
                                    let
                                        newState =
                                            Just (InternalState MouseDrag (newPoints current))

                                        msg =
                                            OnDrag newState (newPoints current)
                                    in
                                    updateDrag newState msg
                                )
                        )
                    , BE.onMouseUp
                        (IO.pageXYDecoder
                            |> JD.map
                                (\current ->
                                    let
                                        newState =
                                            Nothing

                                        msg =
                                            OnEnd Nothing
                                                (newPoints current)
                                                (case type_ of
                                                    MouseDown ->
                                                        Click

                                                    MouseDrag ->
                                                        Drop
                                                )
                                    in
                                    updateDrag newState msg
                                )
                        )
                    ]
                )
                maybeState
    in
    Sub.batch subs
