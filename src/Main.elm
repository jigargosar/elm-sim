module Main exposing (main)

import Browser
import Html exposing (Html, div)
import String exposing (fromFloat)
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Int2 =
    ( Int, Int )


type alias Float2 =
    ( Float, Float )


type alias Model =
    { screenD : Float2
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { screenD = ( 600, 600 )
      }
    , Cmd.none
    )



-- Update


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        () ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    []
        |> Sub.batch



-- view


view : Model -> Html Msg
view model =
    let
        ( width, height ) =
            model.screenD

        w =
            fromFloat width

        h =
            fromFloat height

        x =
            fromFloat (-width / 2)

        y =
            fromFloat (-height / 2)
    in
    svg [ viewBox (x ++ " " ++ y ++ " " ++ w ++ " " ++ h) ] []
