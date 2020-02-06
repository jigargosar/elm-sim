module ReactorSimulation exposing (..)

-- Browser.Element Scaffold

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes



-- Model


type Location
    = Location Int Int


type alias Model =
    { state : Location
    }


type alias Flags =
    ()


initialState =
    Location 0 0


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { state = initialState }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | Reset
    | Step


setState state model =
    { model | state = state }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( setState initialState model, Cmd.none )

        Step ->
            ( { model
                | state = stepState model.state
              }
            , Cmd.none
            )


stepState (Location x y) =
    Location (x + 1) y


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
        [ column [ padding 10 ]
            (Debug.toString model
                |> String.replace ", " "\n ,"
                |> String.replace " }" " \n}"
                |> text
                |> List.singleton
            )
        , row [ spacing 10, padding 10, centerX ]
            [ button (Just Step) "Step"
            , button (Just Reset) "Reset"
            ]
        ]
        |> layout []


button onPress string =
    Input.button
        [ Background.color blue
        , padding 10
        ]
        { onPress = onPress, label = text string }



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
