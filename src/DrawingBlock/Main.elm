module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events as BE
import Html exposing (Html)
import IO
import Json.Decode as JD
import Number2 as N2 exposing (Float2, Int2)
import Svg.Attributes as SA
import Svg.Events as SE
import Task



-- Model


type Mouse
    = Up
    | Down Float2 Float2


type MouseOverElement
    = ZoomElement


type alias Model =
    { browserWH : Float2, zoom : Float2, mouse : Mouse, mouseOver : Maybe MouseOverElement }


setBrowserWH wh m =
    { m | browserWH = wh }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { browserWH = ( 600, 600 )
      , zoom = ( 1, 1 ) |> N2.scale 2.5
      , mouse = Up
      , mouseOver = Nothing
      }
    , IO.getBrowserWH |> Task.perform BrowserResized
      --, Cmd.none
    )



-- Update


type Msg
    = NoOp
    | BrowserResized Float2
    | OnMouseDown Float2
    | OnMouseUp Float2
    | OnMouseMove Float2
    | MouseOverZoom
    | MouseOutZoom


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        BrowserResized wh ->
            ( setBrowserWH wh model, Cmd.none )

        OnMouseDown xy ->
            ( { model | mouse = Down xy xy }, Cmd.none )

        OnMouseUp _ ->
            ( { model | mouse = Up }, Cmd.none )

        OnMouseMove xy ->
            case model.mouse of
                Down startXY _ ->
                    ( { model | mouse = Down startXY xy }, Cmd.none )

                Up ->
                    ( model, Cmd.none )

        MouseOverZoom ->
            ( { model | mouseOver = Just ZoomElement }, Cmd.none )

        MouseOutZoom ->
            if model.mouseOver == Just ZoomElement then
                ( { model | mouseOver = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ IO.onBrowserWH BrowserResized
        , case model.mouse of
            Up ->
                BE.onMouseDown (JD.map OnMouseDown IO.pageXYDecoder)

            Down _ _ ->
                [ BE.onMouseUp (JD.map OnMouseUp IO.pageXYDecoder)
                , BE.onMouseMove (JD.map OnMouseMove IO.pageXYDecoder)
                ]
                    |> Sub.batch
        ]



-- View


view : Model -> Html Msg
view model =
    IO.canvas model.browserWH
        [ [ IO.text ("Zoom = " ++ Debug.toString model.zoom)
                [ SA.id "zoom-element"
                , SE.onMouseOver MouseOverZoom
                ]
          ]
            |> IO.group
                [ IO.transform [ IO.scale2 model.zoom ]
                ]
        ]


empty : Html msg
empty =
    Html.text ""



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
