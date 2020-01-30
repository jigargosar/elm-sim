module DrawingBlock.Draggable exposing
    ( Event(..)
    , State
    , intial
    , mouseBtnTrigger
    , mouseTrigger
    , subscriptions
    )

import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import VirtualDom


type State
    = Waiting
    | BtnDown Int SubState


type SubState
    = NotMoved
    | Moving


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
    mouseEventDecoderWhenBtn btn
        |> JD.map (\_ -> msg (BtnDown btn NotMoved))
        |> IO.stopAllOn "mousedown"


mouseEventDecoderWhenBtn : Int -> Decoder IO.MouseEvent
mouseEventDecoderWhenBtn btn =
    IO.mouseEventDecoder
        |> JD.andThen
            (\e ->
                if e.button == btn then
                    JD.succeed e

                else
                    JD.fail "Not intrested"
            )


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

        BtnDown btn subState ->
            [ mouseEventDecoderWhenBtn btn
                |> JD.map
                    (\me ->
                        updateDrag (BtnDown btn Moving) (OnDrag me)
                    )
                |> BE.onMouseMove
            , mouseEventDecoderWhenBtn btn
                |> JD.map
                    (\me ->
                        updateDrag Waiting
                            (OnEnd me
                                (case subState of
                                    NotMoved ->
                                        Click

                                    Moving ->
                                        Drop
                                )
                            )
                    )
                |> BE.onMouseUp
            ]
                |> Sub.batch
