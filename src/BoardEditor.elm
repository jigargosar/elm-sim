module BoardEditor exposing (..)

-- Browser.Element Scaffold

import Browser
import Element as E
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
        (E.column [] (List.map viewRow (List.range 0 (model.height - 1))))


viewRow r =
    E.row []
        [ E.text ("Row Num: " ++ fromInt (r + 1))
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
