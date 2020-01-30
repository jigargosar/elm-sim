module DrawingBlock.Draggable exposing
    ( Event(..)
    , PageXY
    , Points
    , State
    , delta
    , intial
    , startOnPrimaryMouseDown
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


type alias State =
    Maybe InternalState


type InternalState
    = InternalState MouseState Points


type MouseState
    = MouseDown
    | MouseDrag


type alias Points =
    { start : PageXY
    , prev : PageXY
    , current : PageXY
    }


type Event
    = OnEnd Points End
    | OnDrag Points


intial : State
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


startOnPrimaryMouseDown : (State -> msg) -> VirtualDom.Attribute msg
startOnPrimaryMouseDown msg =
    VirtualDom.on "mousedown"
        (VirtualDom.Custom (fromEvent msg))


delta : Points -> ( Float, Float )
delta { prev, current } =
    ( current, prev ) |> uncurry N2.sub


type End
    = Click
    | Drop


subscriptions : (State -> Event -> msg) -> State -> Sub msg
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
                                            OnDrag (newPoints current)
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
                                            OnEnd
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
