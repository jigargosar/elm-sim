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
    | Down Int Up


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
        |> JD.map (\_ -> msg (Down btn Click))
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
subscriptions updateDrag state =
    case state of
        Waiting ->
            Sub.none

        Down btn up ->
            [ mouseEventDecoderWhenBtn btn
                |> JD.map
                    (\me ->
                        updateDrag (Down btn Drop) (OnDrag me)
                    )
                |> BE.onMouseMove
            , mouseEventDecoderWhenBtn btn
                |> JD.map
                    (\me ->
                        updateDrag Waiting (OnUp up me)
                    )
                |> BE.onMouseUp
            ]
                |> Sub.batch
