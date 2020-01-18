module MirrorPuzzleV2.Mouse2 exposing
    ( Mouse2
    , initial
    , onClick
    , onDrag
    , onDrop
    , update
    )

import Number2 as NT exposing (Float2)
import Playground exposing (Mouse)


onClick : (Float2 -> a) -> Mouse2 -> Maybe a
onClick func (Mouse2 _ e) =
    case e of
        OnClick p ->
            func p |> Just

        _ ->
            Nothing


onDrop : (Float2 -> Float2 -> a) -> Mouse2 -> Maybe a
onDrop func (Mouse2 _ e) =
    case e of
        OnDrop p1 p2 ->
            func p1 p2 |> Just

        _ ->
            Nothing


onDrag : (Float2 -> Float2 -> a) -> Mouse2 -> Maybe a
onDrag func (Mouse2 _ e) =
    case e of
        OnDrag p1 p2 ->
            func p1 p2 |> Just

        _ ->
            Nothing


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
