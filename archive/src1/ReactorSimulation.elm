module ReactorSimulation exposing (..)

-- Browser.Element Scaffold

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes



-- Model


type State
    = State Int


type alias Model =
    Model2


type alias Flags =
    ()


initialLocation =
    State 0


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Simulation [] initialLocation
    , Cmd.none
    )


type Model2
    = Simulation (List State) State
    | History (List State) State (List State)



-- Update


type Msg
    = NoOp
    | Step
    | Undo
    | Redo


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Step ->
            case model of
                Simulation prev current ->
                    ( step prev current
                    , Cmd.none
                    )

                History prev current next ->
                    let
                        func n ( p, c ) =
                            ( c :: p, n )

                        ( newPrev, newCurrent ) =
                            List.foldl func ( prev, current ) next
                    in
                    ( step newPrev newCurrent
                    , Cmd.none
                    )

        Undo ->
            case model of
                Simulation prev current ->
                    ( undo prev current []
                    , Cmd.none
                    )

                History prev current next ->
                    ( undo prev current next
                    , Cmd.none
                    )

        Redo ->
            case model of
                Simulation prev current ->
                    ( redo prev current []
                    , Cmd.none
                    )

                History prev current next ->
                    ( redo prev current next
                    , Cmd.none
                    )


undo prev current next =
    case prev of
        [] ->
            History prev current next

        newCurrent :: newPrev ->
            History newPrev newCurrent (current :: next)


redo prev current next =
    case next of
        [] ->
            History prev current next

        newCurrent :: newNext ->
            History (current :: prev) newCurrent newNext


stepState (State n) =
    State (n + 1)


step prev current =
    Simulation (current :: prev) (stepState current)


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
        [ el [ padding 10 ]
            (viewModel2 model)
        , row [ spacing 10, padding 10, centerX ]
            [ button (Just Step) "Step"
            , button (Just Undo) "Undo"
            , button (Just Redo) "Redo"
            ]
        ]
        |> layout []


viewModel2 model =
    case model of
        Simulation prev current ->
            column [ spacing 10 ]
                [ el [ Font.bold ] (text "SIMULATION View")
                , text ("Current: " ++ Debug.toString current)
                , text ("PrevStateCount: " ++ Debug.toString (List.length prev))
                ]

        History prev current next ->
            column [ spacing 10 ]
                [ el [ Font.bold ] (text "History View")
                , text ("Current: " ++ Debug.toString current)
                , text ("PrevStateCount: " ++ Debug.toString (List.length prev))
                , text ("NextStateCount: " ++ Debug.toString (List.length next))
                ]


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
