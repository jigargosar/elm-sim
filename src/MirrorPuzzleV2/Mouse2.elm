module MirrorPuzzleV2.Mouse2 exposing (Event(..), Model, dragStartPosition, event, initial, update)

import Number2 as NT exposing (Float2)
import Playground exposing (Mouse)


type Model
    = Model State Event


type State
    = Down Int Float2
    | Up


type Event
    = OnClick Float2
    | OnDrag Float2 Float2
    | OnDrop Float2 Float2
    | NoEvent


initial : Model
initial =
    Model Up NoEvent


event : Model -> Event
event (Model _ e) =
    e


dragStartPosition : Model -> Maybe Float2
dragStartPosition =
    event
        >> (\e ->
                case e of
                    OnDrag start _ ->
                        Just start

                    _ ->
                        Nothing
           )


update : Playground.Mouse -> Model -> Model
update mouse (Model state _) =
    let
        current =
            ( mouse.x, mouse.y )

        ( nextState, nextEvent ) =
            case state of
                Up ->
                    if mouse.down then
                        ( Down 0 current, NoEvent )

                    else
                        ( Up, NoEvent )

                Down elapsed startPosition ->
                    let
                        newModel =
                            if mouse.down then
                                Down (elapsed + 1) startPosition

                            else
                                Up

                        newEvent =
                            if elapsed < 60 && NT.equalWithin 3 startPosition current then
                                if mouse.down then
                                    NoEvent

                                else
                                    OnClick startPosition

                            else if mouse.down then
                                OnDrag startPosition current

                            else
                                OnDrop startPosition current
                    in
                    ( newModel, newEvent )
    in
    Model nextState nextEvent
