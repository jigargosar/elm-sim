module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html)
import MainStage as S



-- Model


type alias Model =
    { root : S.Node }


type alias Flags =
    ()


initRootNode =
    let
        ( w, h ) =
            ( 600, 600 )
    in
    S.group
        [ S.rect (w / 2) (h / 4)
        , S.rect (w / 2) (h / 4)
        ]
        |> S.setSize ( 600, 600 )


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { root = initRootNode
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    S.render model.root


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
