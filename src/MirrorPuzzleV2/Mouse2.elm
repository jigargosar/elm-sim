module MirrorPuzzleV2.Mouse2 exposing (Event(..), Mouse2, event, initial, update)

import Number2 as NT exposing (Float2)
import Playground exposing (Mouse)


type Mouse2
    = Mouse2 State Event


type Event
    = OnClick Float2
    | OnDragStart Float2
    | OnDrag Float2 Float2
    | OnDrop Float2 Float2
    | NoEvent


initial : Mouse2
initial =
    Mouse2 Up NoEvent


event : Mouse2 -> Event
event (Mouse2 _ e) =
    e


update : Playground.Mouse -> Mouse2 -> Mouse2
update mouse (Mouse2 state _) =
    let
        ( a, b ) =
            computeState2 ( mouse.x, mouse.y )
                (if mouse.down then
                    IsDown

                 else
                    IsUp
                )
                state
    in
    Mouse2 a b


type State
    = Up
    | DownAndNotDragging Int Float2
    | DownAndDragging Float2


type MouseButton
    = IsDown
    | IsUp


computeState2 : Float2 -> MouseButton -> State -> ( State, Event )
computeState2 currentPosition button previousState =
    let
        tooFarFrom : Float2 -> Bool
        tooFarFrom =
            NT.equalWithin 3 currentPosition >> not

        tooLong : Int -> Bool
        tooLong elapsed =
            elapsed > 60
    in
    case button of
        IsDown ->
            case previousState of
                Up ->
                    ( DownAndNotDragging 0 currentPosition, NoEvent )

                DownAndNotDragging elapsed startPosition ->
                    if tooLong elapsed || tooFarFrom startPosition then
                        ( DownAndDragging startPosition, OnDragStart startPosition )

                    else
                        ( DownAndNotDragging (elapsed + 1) startPosition, NoEvent )

                DownAndDragging startPosition ->
                    ( DownAndDragging startPosition, OnDrag startPosition currentPosition )

        IsUp ->
            ( Up
            , case previousState of
                Up ->
                    NoEvent

                DownAndNotDragging elapsed startPosition ->
                    if tooLong elapsed || tooFarFrom startPosition then
                        NoEvent

                    else
                        OnClick startPosition

                DownAndDragging startPosition ->
                    OnDrop startPosition currentPosition
            )
