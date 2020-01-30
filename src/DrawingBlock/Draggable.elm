module DrawingBlock.Draggable exposing
    ( Event(..)
    , State
    , intial
    , mouseTrigger
    , subscriptions
    )

import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import Maybe.Extra
import VirtualDom


type alias State =
    Maybe InternalState


type InternalState
    = InternalState MouseState


type MouseState
    = MouseDown
    | MouseDrag


type Event
    = OnEnd IO.MouseEvent End
    | OnDrag IO.MouseEvent


intial : State
intial =
    Nothing


mouseTrigger : (State -> msg) -> VirtualDom.Attribute msg
mouseTrigger msg =
    let
        mouseDownStateDecoder : Decoder State
        mouseDownStateDecoder =
            primaryMEDecoder
                |> JD.map
                    (\_ ->
                        Just (InternalState MouseDown)
                    )
    in
    IO.stopAllOn "mousedown" (JD.map msg mouseDownStateDecoder)


type End
    = Click
    | Drop


primaryMEDecoder : Decoder IO.MouseEvent
primaryMEDecoder =
    let
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
    IO.mouseEventDecoder
        |> JD.andThen whenPrimaryMouseButton


subscriptions : (State -> Event -> msg) -> State -> Sub msg
subscriptions updateDrag maybeState =
    let
        subs =
            Maybe.Extra.unwrap []
                (\(InternalState type_) ->
                    [ BE.onMouseMove
                        (primaryMEDecoder
                            |> JD.map
                                (\current ->
                                    let
                                        newState =
                                            Just (InternalState MouseDrag)

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
