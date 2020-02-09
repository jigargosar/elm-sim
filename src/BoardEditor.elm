module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import CD
import Dict exposing (Dict)
import Element as E exposing (padding, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Inst
import Prog
import Program.Builder as B
import Program.Zipper as Z
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


type ReactorInstruction
    = Start
    | In
    | Out
    | Grab
    | Drop
    | NOP


type alias Model =
    { width : Int
    , height : Int
    , dirGrid : DirectionGrid
    , riGrid : GridDict ReactorInstruction
    , edit : Edit
    }


type Edit
    = NoEdit
    | EditDI Int Int
    | EditRI Int Int


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        dirGrid =
            Dict.fromList
                [ ( ( 0, 0 ), Down ) ]

        riGrid =
            Dict.fromList
                [ ( ( 0, 0 ), Start ) ]

        _ =
            Z.init 10 8
                |> Z.switchToBlue
                |> Z.switchToRed
                |> Z.go 4 1
                |> Z.set Inst.Start CD.left
    in
    ( { width = 10
      , height = 8
      , dirGrid = dirGrid
      , riGrid = riGrid
      , edit = NoEdit
      }
    , Cmd.none
    )



-- Update


type Msg
    = NoOp
    | StartEditDI Int Int
    | StartEditRI Int Int
    | DISelected DirectionInstruction
    | RISelected ReactorInstruction


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        StartEditDI x y ->
            ( { model | edit = EditDI x y }, Cmd.none )

        StartEditRI x y ->
            ( { model | edit = EditRI x y }, Cmd.none )

        DISelected di ->
            case model.edit of
                EditDI x y ->
                    ( { model
                        | dirGrid = setDirectionInstruction x y di model.dirGrid
                        , edit = NoEdit
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RISelected ri ->
            case model.edit of
                EditRI x y ->
                    ( { model
                        | riGrid = setRI x y ri model.riGrid
                        , edit = NoEdit
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


directionInstructionAt : Int -> Int -> DirectionGrid -> DirectionInstruction
directionInstructionAt x y =
    Dict.get ( x, y ) >> unwrap DirectionNoChange DirectionChange


riAt : Int -> Int -> RIGrid -> ReactorInstruction
riAt x y =
    Dict.get ( x, y ) >> Maybe.withDefault NOP


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


type alias RIGrid =
    GridDict ReactorInstruction


setRI : Int -> Int -> ReactorInstruction -> RIGrid -> RIGrid
setRI x y =
    Dict.insert ( x, y )


black =
    E.rgb255 0 0 0


lightGray =
    E.rgb255 200 200 200


isEditingDIAt : Int -> Int -> Edit -> Bool
isEditingDIAt x y edit =
    EditDI x y == edit


isEditingRIAt : Int -> Int -> Edit -> Bool
isEditingRIAt x y edit =
    EditRI x y == edit


view : Model -> Html Msg
view model =
    let
        viewCellHelp x y =
            let
                di =
                    directionInstructionAt x y model.dirGrid

                showDIEditor =
                    isEditingDIAt x y model.edit

                ri =
                    riAt x y model.riGrid

                showRIEditor =
                    isEditingRIAt x y model.edit
            in
            viewCell x y di showDIEditor ri showRIEditor
    in
    E.layout []
        (E.column
            [ E.width E.fill
            ]
            [ viewProg
            , renderGrid
                [ Border.width 1
                , Border.color lightGray
                , E.centerX
                , E.width E.shrink
                ]
                model.width
                model.height
                viewCellHelp
            ]
        )


viewProg =
    let
        viewCellHelp x y =
            E.column
                [ Border.width 1
                , Border.color lightGray
                , E.padding 5
                , E.width (E.minimum 80 E.fill)
                , E.height (E.minimum 80 E.fill)
                ]
                [ E.paragraph []
                    [ E.text
                        (Prog.instAt Prog.blue x y prog
                            |> Maybe.map Debug.toString
                            |> Maybe.withDefault ""
                        )
                    ]
                ]

        prog =
            B.init 10 8
                |> B.startAt 4 1
                |> B.step
                |> B.step
                |> B.step
                |> B.exe Inst.grab
                |> B.stepIn CD.Down
                |> B.step
                |> B.step
                |> B.stepIn CD.Left
                |> B.build
                |> Debug.log "b"
    in
    renderGrid
        [ Border.width 1
        , Border.color lightGray
        , E.centerX
        , E.width E.shrink
        ]
        10
        8
        viewCellHelp


viewCell : Int -> Int -> DirectionInstruction -> Bool -> ReactorInstruction -> Bool -> E.Element Msg
viewCell x y di showDIEditor ri showRIEditor =
    let
        viewDI =
            let
                ( diString, diColor ) =
                    case di of
                        DirectionChange d ->
                            ( Debug.toString d, black )

                        DirectionNoChange ->
                            ( "noc", black )
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

        viewRI =
            let
                ( riString, riColor ) =
                    case ri of
                        NOP ->
                            ( "nop", black )

                        _ ->
                            ( Debug.toString ri, black )
            in
            E.el
                [ Font.color riColor
                , E.pointer
                , E.padding 5
                , if showRIEditor then
                    E.inFront (viewRIEditor ri)

                  else
                    Events.onClick (StartEditRI x y)
                ]
                (E.text riString)

        viewIdx =
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
        [ viewIdx
        , viewDI
        , viewRI
        , E.text "HH"
        ]


viewDIEditor di =
    Input.radio
        [ padding 10
        , spacing 20
        , Background.color lightGray
        , Font.color black
        ]
        { onChange = DISelected
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


viewRIEditor : ReactorInstruction -> E.Element Msg
viewRIEditor ri =
    Input.radio
        [ padding 10
        , spacing 20
        , Background.color lightGray
        , Font.color black
        ]
        { onChange = RISelected
        , selected = Just ri
        , label = Input.labelHidden ""
        , options =
            [ Input.option Start (text "Start")
            , Input.option NOP (text "nop")
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
