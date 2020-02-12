module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Set exposing (Set)
import Svg exposing (g, rect, svg)
import Svg.Attributes exposing (fill, stroke)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Types exposing (Transform(..))



-- Model


type alias Int2 =
    ( Int, Int )


type Mask
    = Mask Int (Set Int2)


rotateMask (Mask w set) =
    Set.map (\( x, y ) -> ( y, w - 1 - x )) set
        |> Mask w


lineMask =
    Mask 4 (Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ])


sMask =
    Mask 3 (Set.fromList [ ( 1, 1 ), ( 2, 1 ), ( 0, 2 ), ( 1, 2 ) ])


zMask =
    Mask 3 (Set.fromList [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ])


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
                |> Dict.insert ( 10 - 1, 20 - 1 ) "black"
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


view : Model -> Html Msg
view m =
    let
        cellWidth =
            40
    in
    div [ class "df-row sp10 items-center" ]
        [ [ lineMask, sMask, zMask ]
            |> List.map (viewMaskRotations cellWidth "red")
            |> div [ class "df-row sp10 items-center" ]
        , viewGrid cellWidth m.width m.height m.grid
            |> wrapSvg
        ]


viewMaskRotations cw color mask =
    List.range 0 3
        |> List.map
            (\n ->
                applyN n rotateMask mask
                    |> viewMask cw color
                    |> wrapSvg
            )
        |> div [ class "df-col sp10" ]


applyN : Int -> (c -> c) -> c -> c
applyN n func val =
    List.range 0 (n - 1)
        |> List.foldl (always func) val


wrapSvg s =
    div
        [ style "border" "1px dotted gray"
        , class "lh0"
        ]
        [ s ]


viewMask cw color (Mask maskWidth set) =
    let
        w =
            toFloat maskWidth * cw

        square ( x, y ) =
            rect
                [ width cw
                , height cw
                , fill color
                , strokeWidth 1
                , stroke "white"
                , transform [ Translate (toFloat x * cw) (toFloat y * cw) ]
                ]
                []
    in
    svg [ viewBox 0 0 w w, width w, height w ]
        [ Set.toList set
            |> List.map square
            |> g []
        ]


viewGrid cw gridWidth gridHeight grid =
    let
        square ( x, y ) color =
            rect
                [ width cw
                , height cw
                , fill color
                , transform [ Translate (toFloat x * cw) (toFloat y * cw) ]
                ]
                []

        ( w, h ) =
            ( toFloat gridWidth * cw, toFloat gridHeight * cw )
    in
    svg [ viewBox 0 0 w h, width w, height h ]
        [ Dict.map square grid
            |> Dict.values
            |> g []
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
