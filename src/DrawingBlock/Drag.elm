module DrawingBlock.Drag exposing (Msg, State, subscriptions)

import Basics.Extra exposing (uncurry)
import Browser.Events as BE
import IO
import Json.Decode as JD
import Number2 exposing (Float2)
import PointFree exposing (mapEach)
import Process
import Task
import Time
import VirtualDom


type State
    = Down
    | Drag
    | Up


type Msg
    = OnMouseDown
    | OnDragStart
    | OnDrag
    | OnClick
    | OnDrop


update : Msg -> State -> ( State, Cmd Msg )
update message model =
    case message of
        OnMouseDown ->
            case model of
                Down ->
                    ( Down, Process.sleep 1000 |> Task.perform (\_ -> OnDragStart) )

                _ ->
                    ( model, Cmd.none )

        OnDragStart ->
            case model of
                Down ->
                    ( Drag, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnDrag ->
            ( model, Cmd.none )

        OnClick ->
            ( Up, Cmd.none )

        OnDrop ->
            ( Up, Cmd.none )


tooFar =
    False


subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Up ->
            BE.onMouseDown (JD.succeed OnMouseDown)

        Down ->
            [ BE.onMouseUp (JD.succeed OnClick)
            , BE.onMouseMove
                (if tooFar then
                    JD.succeed OnDragStart

                 else
                    JD.fail "ignoring-move"
                )
            ]
                |> Sub.batch

        Drag ->
            [ BE.onMouseUp (JD.succeed OnDrop)
            , BE.onMouseMove (JD.succeed OnDrag)
            ]
                |> Sub.batch
