module MirrorPuzzleV2.Mouse2 exposing (Event(..), Mouse2, event, initial, update)

import Number2 as NT exposing (Float2)
import Playground exposing (Mouse)


type alias Config a =
    { onClick : Float2 -> a
    , onDragStart : Float2 -> a
    , onDrag : Float2 -> Float2 -> a
    , onDrop : Float2 -> Float2 -> a
    , noEvent : a
    }


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
        ( nextState, nextEvent ) =
            nextStateAndEvent mouse state
    in
    Mouse2 nextState nextEvent


type State
    = Up
    | Down Int Float2
    | Dragging Float2


nextStateAndEvent : Mouse -> State -> ( State, Event )
nextStateAndEvent mouse previousState =
    let
        currentPosition =
            ( mouse.x, mouse.y )

        tooFarFrom : Float2 -> Bool
        tooFarFrom =
            NT.equalWithin 3 currentPosition >> not

        tooLong : Int -> Bool
        tooLong elapsed =
            elapsed > 60
    in
    if mouse.down then
        case previousState of
            Up ->
                ( Down 0 currentPosition, NoEvent )

            Down elapsed startPosition ->
                let
                    continue =
                        Down (elapsed + 1) startPosition
                in
                if tooLong elapsed || tooFarFrom startPosition then
                    ( Dragging startPosition, OnDragStart startPosition )

                else
                    ( continue, NoEvent )

            Dragging startPosition ->
                ( Dragging startPosition, OnDrag startPosition currentPosition )

    else
        ( Up
        , case previousState of
            Up ->
                NoEvent

            Down elapsed startPosition ->
                if tooLong elapsed || tooFarFrom startPosition then
                    NoEvent

                else
                    OnClick startPosition

            Dragging startPosition ->
                OnDrop startPosition currentPosition
        )
