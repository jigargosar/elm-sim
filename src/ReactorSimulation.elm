module ReactorSimulation exposing (..)

-- Browser.Element Scaffold

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes



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


blue =
    Element.rgb255 238 238 238


view : Model -> Html Msg
view model =
    column
        [ centerX, spacing 10 ]
        [ viewState model.state
        , row [ spacing 10, padding 10, centerX ]
            [ button Nothing "Start"
            , button Nothing "Stop"
            ]
        ]
        |> layout []


button onPress string =
    Input.button
        [ Background.color blue
        , padding 10
        ]
        { onPress = onPress, label = text string }


viewState state =
    Element.el
        [ Border.width 2

        --, width shrink
        , centerX
        , padding 10
        ]
        (text ("state: " ++ stateToString state))


stateToString state =
    case state of
        NotStarted ->
            "NotStarted"

        Running ->
            "Running"

        Paused ->
            "Paused"



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Element Helpers
--
--
--
--noinspection ElmUnusedSymbol


noneA =
    Element.htmlAttribute (Html.Attributes.class "")



--noinspection ElmUnusedSymbol


explain =
    Element.explain Debug.todo
