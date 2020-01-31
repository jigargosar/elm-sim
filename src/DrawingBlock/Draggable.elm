module DrawingBlock.Draggable exposing
    ( Event(..)
    , State
    , intial
    , onMouseBtnDown
    , onMouseDown
    , subscriptions
    )

import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import Number2 exposing (Float2)
import VirtualDom


type State
    = Waiting
    | BtnDown Float2 Int End


type End
    = Click
    | Drop


type Event
    = OnEnd End IO.MouseEvent
    | OnDrag Float2 IO.MouseEvent


intial : State
intial =
    Waiting


onMouseDown : (State -> msg) -> VirtualDom.Attribute msg
onMouseDown msg =
    onMouseBtnDown 0 msg


onMouseBtnDown : Int -> (State -> value) -> VirtualDom.Attribute value
onMouseBtnDown btn msg =
    mouseEventDecoderWhenBtn btn
        |> JD.map (\e -> msg (BtnDown e.pageXY btn Click))
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


subscriptions : (State -> Event -> msg) -> State -> Sub msg
subscriptions updateState state =
    case state of
        Waiting ->
            Sub.none

        BtnDown s btn up ->
            let
                updateOnUp e =
                    updateState Waiting (OnEnd up e)

                updateOnDrag e =
                    updateState (BtnDown s btn Drop) (OnDrag s e)

                decoder =
                    mouseEventDecoderWhenBtn btn
            in
            [ BE.onMouseMove (JD.map updateOnDrag decoder)
            , BE.onMouseUp (JD.map updateOnUp decoder)
            ]
                |> Sub.batch
