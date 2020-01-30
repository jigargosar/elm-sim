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
import VirtualDom


type State
    = Waiting
    | SubState Int SubState


type SubState
    = JustDown
    | Dragging


type Event
    = OnEnd IO.MouseEvent End
    | OnDrag IO.MouseEvent


intial : State
intial =
    Waiting


mouseTrigger : (State -> msg) -> VirtualDom.Attribute msg
mouseTrigger msg =
    mouseBtnTrigger 0 msg


mouseBtnTrigger : Int -> (State -> value) -> VirtualDom.Attribute value
mouseBtnTrigger btn msg =
    let
        mouseDownStateDecoder : Decoder State
        mouseDownStateDecoder =
            IO.mouseEventDecoder
                |> JD.andThen
                    (\e ->
                        if e.button == btn then
                            JD.succeed (SubState btn JustDown)

                        else
                            JD.fail "Not intrested"
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
subscriptions updateDrag state =
    case state of
        Waiting ->
            Sub.none

        SubState btn JustDown ->
            [ BE.onMouseMove
                (primaryMEDecoder
                    |> JD.map
                        (\event ->
                            updateDrag (SubState btn Dragging) (OnDrag event)
                        )
                )
            , BE.onMouseUp
                (primaryMEDecoder
                    |> JD.map
                        (\event ->
                            updateDrag Waiting (OnEnd event Click)
                        )
                )
            ]
                |> Sub.batch

        SubState btn Dragging ->
            [ BE.onMouseMove
                (primaryMEDecoder
                    |> JD.map
                        (\current ->
                            updateDrag state (OnDrag current)
                        )
                )
            , BE.onMouseUp
                (primaryMEDecoder
                    |> JD.map
                        (\event ->
                            updateDrag Waiting (OnEnd event Drop)
                        )
                )
            ]
                |> Sub.batch
