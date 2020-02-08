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


type DirectionInstruction
    = DirectionChange Direction
    | DirectionNoChange


type alias Model =
    { width : Int
    , height : Int
    , dirGrid : DirectionGrid
    , edit : Edit
    }


type Edit
    = NoEdit
    | EditDI Int Int


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
    | StartEditDI Int Int
    | DIClicked DirectionInstruction


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StartEditDI x y ->
            ( { model | edit = EditDI x y }, Cmd.none )

        DIClicked di ->
            case model.edit of
                NoEdit ->
                    ( model, Cmd.none )

                EditDI x y ->
                    ( { model
                        | dirGrid = setDirectionInstruction x y di model.dirGrid
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


directionInstructionAt : Int -> Int -> DirectionGrid -> DirectionInstruction
directionInstructionAt x y =
    Dict.get ( x, y ) >> unwrap DirectionNoChange DirectionChange


unwrap default func maybe =
    case maybe of
        Just a ->
            func a

        Nothing ->
            default


setDirectionInstruction : Int -> Int -> DirectionInstruction -> DirectionGrid -> DirectionGrid
setDirectionInstruction x y di =
    case di of
        DirectionNoChange ->
            Dict.remove ( x, y )

        DirectionChange direction ->
            Dict.insert ( x, y ) direction


black =
    E.rgb255 0 0 0


lightGray =
    E.rgb255 200 200 200


isEditingDIAt : Int -> Int -> Edit -> Bool
isEditingDIAt x y edit =
    EditDI x y == edit


view : Model -> Html Msg
view model =
    let
        viewCell x y =
            let
                di =
                    directionInstructionAt x y model.dirGrid

                showDIEditor =
                    isEditingDIAt x y model.edit
            in
            viewCell2 x y di showDIEditor
    in
    E.layout []
        (E.column
            [ E.width E.fill
            ]
            [ renderGrid
                [ Border.width 1
                , Border.color lightGray
                , E.centerX
                , E.width E.shrink
                ]
                model.width
                model.height
                viewCell
            ]
        )


viewCell2 x y di showDIEditor =
    let
        dirEl =
            let
                ( diString, diColor ) =
                    case di of
                        DirectionChange d ->
                            ( Debug.toString d, black )

                        DirectionNoChange ->
                            ( "noc", lightGray )
            in
            E.el
                [ Font.color diColor
                , E.pointer
                , E.padding 5
                , if showDIEditor then
                    E.inFront (viewDIEditor di)

                  else
                    Events.onClick (StartEditDI x y)
                ]
                (E.text diString)

        indexEl =
            E.el
                [ E.padding 5
                , Font.size 14
                , Font.color lightGray
                ]
                (E.text (fromInt x ++ "," ++ fromInt y))
    in
    E.column
        [ Border.width 1
        , Border.color lightGray
        , E.padding 5
        , E.width (E.minimum 80 E.fill)
        ]
        [ indexEl
        , dirEl
        ]


viewDIEditor di =
    Input.radio
        [ padding 10
        , spacing 20
        , Background.color lightGray
        , Font.color black
        ]
        { onChange = DIClicked
        , selected = Just di
        , label = Input.labelHidden ""
        , options =
            [ Input.option (DirectionChange Up) (text "Up")
            , Input.option (DirectionChange Down) (text "Down")
            , Input.option (DirectionChange Left) (text "Left")
            , Input.option (DirectionChange Right) (text "Right")
            , Input.option DirectionNoChange (text "noc")
            ]
        }


renderGrid attrs width height viewFunc =
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
