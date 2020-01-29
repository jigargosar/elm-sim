module DrawingBlock.Drag exposing (Msg, State, onDown, subscriptions)

import Basics.Extra exposing (uncurry)
import Browser.Events as BE
import IO
import Json.Decode as JD
import Number2 exposing (Float2)
import PointFree exposing (mapEach)
import Task
import VirtualDom


type EventData
    = EventData
        -- pageXY
        Float2
        -- timeStamp
        Float


type State
    = Down
        -- start
        EventData
        -- current
        EventData
    | Up


type Msg
    = OnMouseDown EventData
    | OnMouseUp EventData
    | OnMouseMove EventData


type alias EventDiff =
    { dx : Float
    , dy : Float
    , isDragging : Bool
    }


eventDiff : EventData -> EventData -> EventDiff
eventDiff (EventData sxy st) (EventData xy t) =
    let
        elapsed =
            ( t, st ) |> uncurry (-)

        tooLong =
            abs elapsed > (3 * 1000)

        dx =
            ( xy, sxy ) |> mapEach Tuple.first |> uncurry (-)

        dy =
            ( xy, sxy ) |> mapEach Tuple.second |> uncurry (-)

        tooFar =
            abs dx > 10 || abs dy > 10

        isDragging =
            tooLong || tooFar
    in
    EventDiff dx dy isDragging


update config message state =
    case message of
        OnMouseDown ed ->
            ( Down ed ed, config.onDown |> Task.perform identity )

        OnMouseUp ed ->
            ( Up
            , case state of
                Up ->
                    Cmd.none

                Down startED _ ->
                    let
                        edDiff =
                            eventDiff startED ed
                    in
                    if edDiff.isDragging then
                        config.onDrop |> Task.perform identity

                    else
                        config.onClick |> Task.perform identity
            )

        OnMouseMove ed ->
            case state of
                Up ->
                    ( Up, Cmd.none )

                Down startEd _ ->
                    ( Down startEd ed
                    , let
                        edDiff =
                            eventDiff startEd ed
                      in
                      if edDiff.isDragging then
                        config.onDrag |> Task.perform identity

                      else
                        Cmd.none
                    )


eventDataDecoder : JD.Decoder EventData
eventDataDecoder =
    JD.map2 EventData IO.pageXYDecoder IO.timeStampDecoder


onDown : (Msg -> msg) -> VirtualDom.Attribute msg
onDown toMsg =
    VirtualDom.on "down"
        (VirtualDom.Normal
            (JD.map OnMouseDown eventDataDecoder)
        )
        |> VirtualDom.mapAttribute toMsg


subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Up ->
            BE.onMouseDown (JD.map OnMouseDown eventDataDecoder)

        Down _ _ ->
            [ BE.onMouseUp (JD.map OnMouseUp eventDataDecoder)
            , BE.onMouseMove (JD.map OnMouseMove eventDataDecoder)
            ]
                |> Sub.batch
