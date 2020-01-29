module DrawingBlock.Drag exposing (State, onDown)

import Browser.Events as BE
import IO
import Json.Decode as JD
import Number2 exposing (Float2)
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


subscriptions state =
    case state of
        Up ->
            BE.onMouseDown (JD.map OnMouseDown eventDataDecoder)

        Down _ _ ->
            [ BE.onMouseUp (JD.map OnMouseUp eventDataDecoder)
            , BE.onMouseMove (JD.map OnMouseMove eventDataDecoder)
            ]
                |> Sub.batch
