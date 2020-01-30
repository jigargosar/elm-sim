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
import VirtualDom


type State
    = Waiting
    | BtnDown Int Up


type Up
    = Click
    | Drop


type Event
    = OnUp Up IO.MouseEvent
    | OnDrag IO.MouseEvent


intial : State
intial =
    Waiting


onMouseDown : (State -> msg) -> VirtualDom.Attribute msg
onMouseDown msg =
    onMouseBtnDown 0 msg


onMouseBtnDown : Int -> (State -> value) -> VirtualDom.Attribute value
onMouseBtnDown btn msg =
    mouseEventDecoderWhenBtn btn
        |> JD.map (\_ -> msg (BtnDown btn Click))
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

        BtnDown btn up ->
            let
                updateOnUp e =
                    updateState Waiting (OnUp up e)

                updateOnDrag e =
                    updateState (BtnDown btn Drop) (OnDrag e)

                decoder =
                    mouseEventDecoderWhenBtn btn
            in
            [ BE.onMouseMove (JD.map updateOnDrag decoder)
            , BE.onMouseUp (JD.map updateOnUp decoder)
            ]
                |> Sub.batch
