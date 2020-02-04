module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import MainStage as S
import Task



-- Model


type alias Model =
    { root : S.Node }


type alias Flags =
    ()


initRootNode ( w, h ) =
    S.group
        [ S.rect (w / 2) (h / 4)
        , S.rect (w / 2) (h / 4)
        ]
        |> S.setSize ( w, h )


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            ( 600, 600 )
    in
    ( { root = initRootNode size
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | root = initRootNode ( scene.width, scene.height ) }, Cmd.none )

        OnResize _ _ ->
            ( model, Browser.Dom.getViewport |> Task.perform GotViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnResize
        ]



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
