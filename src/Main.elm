module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div)
import Svg exposing (g, rect, svg)
import Svg.Attributes exposing (fill)
import Tuple exposing (mapBoth)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Types exposing (Transform(..))



-- Model


type alias Int2 =
    ( Int, Int )


type alias Model =
    { grid : Dict Int2 String
    , width : Int
    , height : Int
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            Dict.empty
                |> Dict.insert ( 0, 0 ) "blue"
                |> Dict.insert ( 1, 1 ) "red"
                |> Dict.insert ( 10, 20 ) "black"
      , width = 10
      , height = 20
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


cellWidth =
    50


scaleInt2 =
    mapEach (toFloat >> mul cellWidth)


square pos color =
    let
        ( x, y ) =
            pos |> scaleInt2
    in
    rect [ width cellWidth, height cellWidth, fill color, transform [ Translate x y ] ]
        []


view : Model -> Html Msg
view m =
    let
        ( w, h ) =
            ( m.width, m.height ) |> scaleInt2
    in
    svg [ viewBox 0 0 w h ]
        [ g [] (renderGrid m.grid)
        ]


renderGrid : Dict ( Int, Int ) String -> List (Svg.Svg msg)
renderGrid grid =
    Dict.map square grid
        |> Dict.values


mul =
    (*)


mapEach func =
    mapBoth func func


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
