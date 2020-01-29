module DrawingBlock.Drag exposing (Msg, State, subscriptions)

import Browser.Events as BE
import Json.Decode as JD


type State
    = Down
    | Drag
    | Up


type Msg
    = OnMouseDown
    | OnDrag
    | OnClick
    | OnDrop


update : Msg -> State -> State
update message model =
    case message of
        OnMouseDown ->
            Down

        OnDrag ->
            Drag

        OnClick ->
            Up

        OnDrop ->
            Up


subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Up ->
            BE.onMouseDown (JD.succeed OnMouseDown)

        Down ->
            [ BE.onMouseUp (JD.succeed OnClick)
            , BE.onMouseMove (JD.succeed OnDrag)
            ]
                |> Sub.batch

        Drag ->
            [ BE.onMouseUp (JD.succeed OnDrop)
            , BE.onMouseMove (JD.succeed OnDrag)
            ]
                |> Sub.batch
