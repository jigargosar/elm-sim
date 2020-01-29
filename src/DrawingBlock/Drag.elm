module DrawingBlock.Drag exposing
    ( Drag
    , Event
    , Msg
    , OutMsg(..)
    , intial
    , onDown
    , subscriptions
    , update
    )

import Basics.Extra exposing (uncurry)
import Browser.Events as BE
import IO
import Json.Decode as JD exposing (Decoder)
import Number2 as N2 exposing (Float2)
import PointFree exposing (mapEach)
import VirtualDom


type alias Event =
    { pageXY : Float2 }


eventDecoder : Decoder Event
eventDecoder =
    JD.map Event IO.pageXYDecoder


type Drag
    = Drag (Maybe State)


type alias State =
    { start : Event
    , prev : Event
    , current : Event
    , type_ : EventType
    }


type EventType
    = MouseDown
    | MouseDrag


type Msg
    = OnMove Event
    | OnUp Event


intial : Drag
intial =
    Drag Nothing


fromEvent e =
    Drag (Just (State e e e MouseDown))


onDown : (Drag -> msg) -> VirtualDom.Attribute msg
onDown msg =
    VirtualDom.on "mousedown"
        (VirtualDom.Normal
            (JD.map (fromEvent >> msg) eventDecoder)
        )


delta : State -> ( Float, Float )
delta { current, prev } =
    ( current, prev ) |> (mapEach .pageXY >> uncurry N2.sub)


type OutMsg
    = Move Float2
    | Click
    | End


update : Msg -> Drag -> ( Drag, OutMsg )
update message ((Drag maybeState) as model) =
    case message of
        OnMove event ->
            case maybeState of
                Nothing ->
                    ( model, End )

                Just state ->
                    let
                        newState =
                            { state | prev = state.current, current = event, type_ = MouseDrag }
                    in
                    ( newState |> Just |> Drag, Move (delta newState) )

        OnUp _ ->
            case maybeState of
                Nothing ->
                    ( model, End )

                Just { type_ } ->
                    ( Drag Nothing
                    , case type_ of
                        MouseDown ->
                            Click

                        MouseDrag ->
                            End
                    )


subscriptions : Drag -> Sub Msg
subscriptions (Drag maybeState) =
    let
        decoder x =
            JD.map x eventDecoder

        subs =
            case maybeState of
                Nothing ->
                    []

                _ ->
                    [ BE.onMouseUp (decoder OnUp)
                    , BE.onMouseMove (decoder OnMove)
                    ]
    in
    Sub.batch subs
