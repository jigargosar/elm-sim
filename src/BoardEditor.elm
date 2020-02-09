port module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import CD
import Dict exposing (Dict)
import Element as E exposing (none, padding, paddingXY, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Inst
import Prog exposing (LayerName)
import Program.Builder as B
import Program.Zipper as Z
import String exposing (fromInt)
import Task


port getScrollbarSize : () -> Cmd msg


port gotScrollbarSize : (( Int, Int ) -> msg) -> Sub msg



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
    , showDialog : Bool
    , scrollbarSize : ( Int, Int )
    , dialog : Dialog
    }


type Dialog
    = ArrowDialog LayerName Int Int
    | NoDialog


type Edit
    = NoEdit
    | EditDI Int Int
    | EditRI Int Int


type alias Flags =
    { scrollbarSize : ( Int, Int )
    , now : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        _ =
            Debug.log "flags" flags

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
      , showDialog = False
      , scrollbarSize = flags.scrollbarSize
      , dialog = NoDialog
      }
    , Cmd.batch
        [ Browser.Dom.getViewport |> Task.perform (Debug.log "vp" >> always NoOp)
        , getScrollbarSize ()
        ]
    )



-- Update


type Msg
    = NoOp
    | StartEditDI Int Int
    | StartEditRI Int Int
    | DISelected DirectionInstruction
    | RISelected ReactorInstruction
    | ToggleDialog
    | ShowArrowDialog Prog.LayerName Int Int
    | DialogBackgroundClicked
    | GotScrollbarSize Int2


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

        ToggleDialog ->
            ( { model | showDialog = not model.showDialog }, Cmd.none )

        GotScrollbarSize s ->
            ( { model | scrollbarSize = s }, Cmd.none )

        ShowArrowDialog layerName x y ->
            ( { model | dialog = ArrowDialog layerName x y }, Cmd.none )

        DialogBackgroundClicked ->
            ( { model | dialog = NoDialog }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotScrollbarSize (Debug.log "gotScrollbarSize" >> GotScrollbarSize)
        ]



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


blue =
    E.rgb255 49 98 179


red =
    E.rgb255 179 17 25


white =
    E.rgb255 255 255 255


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
    E.layout
        [ E.inFront
            (viewDialog model)
        , E.height E.fill
        ]
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


viewDialog model =
    case model.dialog of
        NoDialog ->
            none

        ArrowDialog layerName x y ->
            dialogContainer model.scrollbarSize viewArrowDialogContent


dialogContainer ( scrollbarXWidth, _ ) content =
    E.el
        [ E.width E.fill
        , E.height E.fill
        , Background.color (E.rgba 0 0 0 0.5)
        , E.inFront
            (E.el
                [ E.centerX
                , E.centerY
                , Background.color white
                , Font.color black
                , Border.rounded 10
                , paddingXY 0 10
                ]
                content
            )
        , E.below
            (E.html
                (Html.node "style"
                    []
                    [ Html.text <|
                        "body{overflow:hidden;padding-right:"
                            ++ fromInt scrollbarXWidth
                            ++ "px}"
                    ]
                )
            )
        ]
        (E.el [ E.width E.fill, E.height E.fill, Events.onClick DialogBackgroundClicked ] none)


viewArrowDialogContent =
    E.column
        [ padding 10
        , E.width (E.shrink |> E.minimum 200)
        , E.height (E.shrink |> E.minimum 200)
        , E.scrollbars
        ]
        [ E.text "ArrowDialog"
        ]


viewProg =
    let
        viewCellHelp x y =
            E.row
                [ Border.width 1
                , Border.color lightGray
                , E.padding 5
                , E.width (E.minimum 80 E.fill)
                , E.height (E.minimum 80 E.fill)
                , E.spacing 10
                ]
                [ E.column
                    [ Font.color red
                    , E.width E.fill
                    , spacing 5
                    , E.alignTop
                    ]
                    [ E.text
                        (Prog.instAt Prog.red x y prog
                            |> Maybe.map Debug.toString
                            |> Maybe.withDefault " "
                        )
                        |> E.el [ E.centerX ]
                    , E.text
                        (Prog.arrowAt Prog.red x y prog
                            |> Maybe.map CD.arrowSymbol
                            |> Maybe.withDefault " "
                        )
                        |> E.el [ E.centerX, Events.onClick (ShowArrowDialog Prog.red x y) ]
                    ]
                , E.column
                    [ Font.color blue
                    , E.width E.fill
                    , spacing 5
                    , E.alignBottom
                    ]
                    [ E.text
                        (Prog.instAt Prog.blue x y prog
                            |> Maybe.map Debug.toString
                            |> Maybe.withDefault " "
                        )
                        |> E.el
                            [ E.centerX
                            , Events.onClick ToggleDialog
                            ]
                    , E.text
                        (Prog.arrowAt Prog.blue x y prog
                            |> Maybe.map CD.arrowSymbol
                            |> Maybe.withDefault " "
                        )
                        |> E.el [ E.centerX, Events.onClick (ShowArrowDialog Prog.blue x y) ]
                    ]
                ]

        prog : Prog.Prog
        prog =
            B.build
                { width = 10
                , height = 8
                , red =
                    { x = 4
                    , y = 1
                    , arrow = CD.Left
                    , steps =
                        [ B.exe Inst.alphaInput
                        , B.step
                        , B.exe2 Inst.grab CD.Down
                        , B.step
                        , B.step
                        , B.stepIn CD.Right
                        , B.step
                        , B.step
                        , B.step
                        , B.step
                        , B.step
                        , B.stepIn CD.Up
                        , B.exe Inst.drop
                        , B.exe Inst.psiOutput
                        , B.stepIn CD.Left
                        ]
                    }
                , blue =
                    { x = 4
                    , y = 1
                    , arrow = CD.Left
                    , steps =
                        [ B.exe Inst.alphaInput
                        ]
                    }
                }
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
