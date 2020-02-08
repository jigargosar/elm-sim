module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Element as E
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import String exposing (fromInt)



-- Model


type alias Int2 =
    ( Int, Int )


type alias GridDict a =
    Dict Int2 a


type alias DirectionGrid =
    GridDict Direction


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { width : Int
    , height : Int
    , dirGrid : DirectionGrid
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { width = 10
      , height = 8
      , dirGrid = Dict.empty
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


getAt : Int -> Int -> GridDict a -> Maybe a
getAt x y =
    Dict.get ( x, y )


dirAt : Int -> Int -> DirectionGrid -> Maybe Direction
dirAt =
    getAt


view : Model -> Html Msg
view model =
    let
        viewCell x y =
            let
                maybeDir =
                    dirAt x y model.dirGrid
            in
            case maybeDir of
                Nothing ->
                    E.el
                        [ E.padding 10
                        , Font.center
                        , Border.width 1
                        ]
                        (E.text (fromInt x ++ "," ++ fromInt y))

                Just dir ->
                    E.el
                        [ E.padding 10
                        , Font.center
                        , Border.width 1
                        ]
                        (E.text (Debug.toString dir))
    in
    E.layout []
        (E.column []
            [ viewTable model.width model.height viewCell
            ]
        )


viewTable width height viewFunc =
    let
        column x =
            E.Column E.none E.fill (viewFunc x)
    in
    E.table
        [ Border.width 1
        ]
        { data = List.range 0 (height - 1)
        , columns =
            List.range 0 (width - 1)
                |> List.map column
        }



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
