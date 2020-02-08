module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Element as E exposing (padding, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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


type DirOption
    = DirOption Direction
    | NOC


type alias Model =
    { width : Int
    , height : Int
    , dirGrid : DirectionGrid
    , edit : Edit
    }


type Edit
    = NoEdit
    | EditDir Int Int DirOption


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
      , edit = NoEdit
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | StartEditDir Int Int DirOption
    | DirOptClicked DirOption


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StartEditDir x y dirOption ->
            ( { model | edit = EditDir x y dirOption }, Cmd.none )

        DirOptClicked dirOption ->
            case model.edit of
                NoEdit ->
                    ( model, Cmd.none )

                EditDir x y _ ->
                    case dirOption of
                        NOC ->
                            ( { model
                                | dirGrid = setNOCDirAt x y model.dirGrid
                                , edit = NoEdit
                              }
                            , Cmd.none
                            )

                        DirOption direction ->
                            ( { model
                                | dirGrid = setDirAt x y direction model.dirGrid
                                , edit = NoEdit
                              }
                            , Cmd.none
                            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


setAt : Int -> Int -> a -> GridDict a -> GridDict a
setAt x y a =
    Dict.insert ( x, y ) a


getAt : Int -> Int -> GridDict a -> Maybe a
getAt x y =
    Dict.get ( x, y )


removeAt : Int -> Int -> GridDict a -> GridDict a
removeAt x y =
    Dict.remove ( x, y )


dirAt : Int -> Int -> DirectionGrid -> Maybe Direction
dirAt =
    getAt


setNOCDirAt : Int -> Int -> DirectionGrid -> DirectionGrid
setNOCDirAt =
    removeAt


setDirAt : Int -> Int -> Direction -> DirectionGrid -> DirectionGrid
setDirAt =
    setAt


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
                        common dirOpt =
                            [ E.pointer

                            --, Border.width 1
                            , E.width E.fill
                            , E.height E.fill
                            , E.padding 5
                            , case model.edit of
                                EditDir x_ y_ dirOpt_ ->
                                    if x == x_ && y == y_ then
                                        E.below (rad dirOpt_)

                                    else
                                        Events.onClick (StartEditDir x y dirOpt)

                                _ ->
                                    Events.onClick (StartEditDir x y dirOpt)
                            ]

                        rad dirOpt =
                            Input.radio
                                [ padding 10
                                , spacing 20
                                , Background.color lightGray
                                , Font.color black
                                , E.moveRight 9
                                ]
                                { onChange = DirOptClicked
                                , selected = Just dirOpt
                                , label = Input.labelHidden ""
                                , options =
                                    [ Input.option (DirOption Up) (text "Up")
                                    , Input.option (DirOption Down) (text "Down")
                                    , Input.option (DirOption Left) (text "Left")
                                    , Input.option (DirOption Right) (text "Right")
                                    , Input.option NOC (text "noc")
                                    ]
                                }
                    in
                    case dirAt x y model.dirGrid of
                        Just dir ->
                            E.el
                                (Font.color black
                                    :: common (DirOption dir)
                                )
                                (E.text (Debug.toString dir))

                        Nothing ->
                            E.el
                                (Font.color lightGray
                                    :: common NOC
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
                , E.width (E.minimum 80 E.fill)
                , E.height (E.minimum 80 E.fill)
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
