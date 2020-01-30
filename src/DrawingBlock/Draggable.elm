module DrawingBlock.Draggable exposing
    ( Event(..)
    , MouseEvent
    , PageXY
    , State
    , intial
    , primartMouseTrigger
    , subscriptions
    )

import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import Maybe.Extra
import Number2 exposing (Float2)
import VirtualDom


type alias PageXY =
    Float2


type alias State =
    Maybe InternalState


type InternalState
    = InternalState MouseState PageXY


type MouseState
    = MouseDown
    | MouseDrag


type alias MouseEvent =
    { pageXY : Float2
    , movementXY : Float2
    }


type Event
    = OnEnd MouseEvent End
    | OnDrag MouseEvent


intial : State
intial =
    Nothing


type alias CustomHandler msg =
    { message : msg, preventDefault : Bool, stopPropagation : Bool }


stopAll : a -> CustomHandler a
stopAll msg =
    CustomHandler msg True True


mouseDownStateDecoder : Decoder State
mouseDownStateDecoder =
    primaryMEDecoder
        |> JD.map
            (\e ->
                Just (InternalState MouseDown e.pageXY)
            )


primartMouseTrigger : (State -> msg) -> VirtualDom.Attribute msg
primartMouseTrigger msg =
    VirtualDom.on "mousedown"
        (VirtualDom.Custom
            (mouseDownStateDecoder
                |> JD.map (msg >> stopAll)
            )
        )


type End
    = Click
    | Drop


primaryMEDecoder : Decoder MouseEvent
primaryMEDecoder =
    let
        mouseEventDecoder : Decoder MouseEvent
        mouseEventDecoder =
            JD.map2 MouseEvent IO.pageXYDecoder IO.movementXYDecoder

        whenPrimaryMouseButton : a -> Decoder a
        whenPrimaryMouseButton msg =
            JD.field "button" JD.int
                |> JD.andThen
                    (\button ->
                        if button == 0 then
                            JD.succeed msg

                        else
                            JD.fail "not primary button"
                    )
    in
    mouseEventDecoder
        |> JD.andThen whenPrimaryMouseButton


subscriptions : (State -> Event -> msg) -> State -> Sub msg
subscriptions updateDrag maybeState =
    let
        subs =
            Maybe.Extra.unwrap []
                (\(InternalState type_ start) ->
                    [ BE.onMouseMove
                        (primaryMEDecoder
                            |> JD.map
                                (\current ->
                                    let
                                        newState =
                                            Just (InternalState MouseDrag start)

                                        msg =
                                            OnDrag current
                                    in
                                    updateDrag newState msg
                                )
                        )
                    , BE.onMouseUp
                        (primaryMEDecoder
                            |> JD.map
                                (\current ->
                                    let
                                        newState =
                                            Nothing

                                        msg =
                                            OnEnd
                                                current
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
