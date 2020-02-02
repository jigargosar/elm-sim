module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Svg as S exposing (svg)
import Svg.Attributes exposing (fill)
import Task
import Tuple exposing (mapBoth)
import TypedSvg.Attributes as TA exposing (viewBox)


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
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = OnResize Int Int
    | GotViewport Browser.Dom.Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnResize w h ->
            ( { model | screenD = ( w, h ) |> toFloat2 }, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenD = ( scene.width, scene.height ) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onResize OnResize ]
        |> Sub.batch



-- view


view : Model -> Html Msg
view model =
    canvas model.screenD
        []
        [ square "dodgerblue" 100 []
        ]


canvas : Float2 -> List (S.Attribute a) -> List (S.Svg a) -> Html a
canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg (viewBox x y w h :: attrs)


square : String -> Float -> List (S.Attribute msg) -> S.Svg msg
square c w =
    rect c w w


rect color width height attrs =
    let
        ( x, y ) =
            ( width / 2, height / 2 )
    in
    S.polygon
        (TA.points [ ( -x, -y ), ( x, -y ), ( x, y ), ( -x, y ) ]
            :: fill color
            :: attrs
        )
        []
