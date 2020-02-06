module ReactorSimulation exposing (..)

-- Browser.Element Scaffold

import Browser
import Html exposing (Html, div, text)



-- Model


type Location
    = Location


type State
    = Running
    | Paused
    | NotStarted


type alias Model =
    { initialLocation : Location
    , state : State
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { initialLocation = Location, state = NotStarted }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | Play
    | Pause
    | Reset


setState state model =
    { model | state = state }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Play ->
            ( setState Running model, Cmd.none )

        Pause ->
            ( setState Paused model, Cmd.none )

        Reset ->
            ( setState NotStarted model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    div [] [ viewState model.state ]


viewState state =
    div [] [ text (stateToString state) ]


stateToString state =
    case state of
        NotStarted ->
            "NotStarted"

        Running ->
            "Running"

        Paused ->
            "Paused"


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
