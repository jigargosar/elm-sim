module Gravitron.Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, text)
import Random exposing (Seed)



-- Model


type alias Flags =
    { now : Int }


type alias Model =
    { seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = Random.initialSeed flags.now }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view _ =
    text "hi"


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
