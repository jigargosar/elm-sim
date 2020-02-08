module BoardEditor exposing (..)

-- Browser.Element Scaffold

import Browser
import Element as E
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import String exposing (fromInt)



-- Model


type alias Model =
    { width : Int
    , height : Int
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { width = 10
      , height = 8
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
    E.layout []
        -- (E.column [] (List.map (viewRow model.height) (List.range 0 (model.height - 1))))
        (E.column [] [ viewTable model.width model.height ])


viewTable width height =
    let
        viewCell x y =
            E.el
                [ E.padding 10
                , Font.center
                , Border.width 1
                ]
                (E.text (fromInt x ++ "," ++ fromInt y))

        column x =
            E.Column E.none E.fill (viewCell x)
    in
    E.table
        [ Border.width 1
        ]
        { data = List.range 0 (height - 1)
        , columns =
            List.range 0 (width - 1)
                |> List.map column
        }


viewRow height y =
    let
        viewCell x =
            E.text ("(" ++ fromInt y ++ "," ++ fromInt x ++ ")")
    in
    E.row []
        (List.map viewCell (List.range 0 (height - 1)))


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
