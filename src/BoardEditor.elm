module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Element as E
import Element.Border as Border
import Element.Events as Events
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
    let
        dirGrid =
            Dict.fromList
                [ ( ( 0, 0 ), Down ) ]
    in
    ( { width = 10
      , height = 8
      , dirGrid = dirGrid
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | DirClicked Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        DirClicked x y ->
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


black =
    E.rgb255 0 0 0


lightGray =
    E.rgb255 200 200 200


view : Model -> Html Msg
view model =
    let
        viewCell x y =
            let
                dirEl =
                    let
                        common =
                            [ E.pointer

                            --, Border.width 1
                            , E.width E.fill
                            , E.height E.fill
                            , E.padding 5
                            , Events.onClick (DirClicked x y)
                            ]
                    in
                    case dirAt x y model.dirGrid of
                        Just dir ->
                            E.el
                                (Font.color black
                                    :: common
                                )
                                (E.text (Debug.toString dir))

                        Nothing ->
                            E.el
                                (Font.color lightGray
                                    :: common
                                )
                                (E.text "noc")

                indexStr =
                    fromInt x ++ "," ++ fromInt y

                indexEl =
                    E.el
                        [ E.padding 5
                        , Font.size 14
                        , Font.color lightGray
                        ]
                        (E.text indexStr)
            in
            E.column
                [ Border.width 1
                , Border.color lightGray
                , E.padding 5
                , E.height E.fill
                ]
                [ indexEl
                , dirEl
                ]
    in
    E.layout []
        (E.column []
            [ viewTable
                [ Border.width 1
                , Border.color lightGray
                ]
                model.width
                model.height
                viewCell
            ]
        )


viewTable attrs width height viewFunc =
    let
        column x =
            E.Column E.none E.fill (viewFunc x)
    in
    E.table attrs
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
