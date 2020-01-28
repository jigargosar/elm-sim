module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import IO
import Number2 as N2 exposing (Float2, Int2)
import String2 as ST
import Svg as S
import Svg.Attributes as SA
import Task



-- Model


type alias Model =
    { browserWH : Float2, zoom : Float2 }


setBrowserWH wh m =
    { m | browserWH = wh }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { browserWH = ( 600, 600 )
      , zoom = ( 1, 1 ) |> N2.scale 2.5
      }
    , IO.getBrowserWH |> Task.perform BrowserResized
      --, Cmd.none
    )



-- Update


type Msg
    = NoOp
    | BrowserResized Float2


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        BrowserResized wh ->
            ( setBrowserWH wh model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ IO.onBrowserWH BrowserResized
        ]



-- View


view : Model -> Html Msg
view model =
    IO.canvas model.browserWH
        [ [ IO.text ("Zoom = " ++ Debug.toString model.zoom) []
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
