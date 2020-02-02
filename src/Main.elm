module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div)
import String exposing (fromFloat)
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)
import Tuple exposing (mapBoth)


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


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach f =
    mapBoth f f


toFloat2 : Int2 -> Float2
toFloat2 =
    mapEach toFloat


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


type Msg
    = OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnResize w h ->
            ( { model | screenD = ( w, h ) |> toFloat2 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onResize OnResize ]
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
