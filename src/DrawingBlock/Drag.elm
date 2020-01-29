module DrawingBlock.Drag exposing (Model, Msg, subscriptions)

import Browser.Events as BE
import Json.Decode as JD


type Model a
    = Down a
    | Drag a
    | Up


type Msg a
    = OnDown a
    | OnDrag a
    | OnClick a
    | OnDrop a


update : Msg a -> Model a -> Model a
update message model =
    case message of
        OnDown a ->
            Down a

        OnDrag a ->
            Drag a

        OnClick _ ->
            Up

        OnDrop _ ->
            Up


subscriptions : Model a -> Sub (Msg a)
subscriptions state =
    case state of
        Up ->
            Sub.none

        Down a ->
            [ BE.onMouseUp (JD.succeed (OnClick a))
            , BE.onMouseMove (JD.succeed (OnDrag a))
            ]
                |> Sub.batch

        Drag a ->
            [ BE.onMouseUp (JD.succeed (OnDrop a))
            , BE.onMouseMove (JD.succeed (OnDrag a))
            ]
                |> Sub.batch
