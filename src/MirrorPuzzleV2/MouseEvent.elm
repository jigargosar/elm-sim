module MirrorPuzzleV2.MouseEvent exposing (..)

import Number2 as NT exposing (Float2)
import Playground exposing (Mouse)


type Model
    = Down Int Float2
    | Up


type Event
    = OnClick Float2
    | OnDrag Float2 Float2
    | OnDrop Float2 Float2
    | NoEvent


toEvent : Mouse -> Model -> ( Model, Event )
toEvent mouse model =
    let
        current =
            ( mouse.x, mouse.y )
    in
    case model of
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
