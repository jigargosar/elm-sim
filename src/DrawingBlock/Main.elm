module DrawingBlock.Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import IO
import Number2 exposing (Float2, Int2)
import String2 as ST
import Svg as S
import Svg.Attributes as SA



-- Model


type alias Model =
    { browserWH : Float2, scaleXY : Float2 }


setBrowserWH wh m =
    { m | browserWH = wh }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { browserWH = ( 600, 600 )
      , scaleXY = ( 1, 1 )
      }
      --, IO.getBrowserWH |> Task.perform BrowserResized
    , Cmd.none
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


scale2 sxy =
    let
        ( x, y ) =
            ST.fromFloat sxy
    in
    "scale(" ++ x ++ "," ++ y ++ ")"


view : Model -> Html Msg
view model =
    IO.canvas model.browserWH
        [ S.g
            [ scale2 model.scaleXY |> SA.transform
            ]
            [ S.text_ [] [ S.text "HW" ] ]
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
