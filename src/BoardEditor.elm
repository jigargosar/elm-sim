port module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import CD
import Element as E exposing (none, padding, paddingXY, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Inst
import Prog exposing (LayerName)
import Program.Builder as B
import String exposing (fromInt)
import Task


port getScrollbarSize : () -> Cmd msg


port gotScrollbarSize : (( Int, Int ) -> msg) -> Sub msg



-- Model


type alias Int2 =
    ( Int, Int )


type alias Model =
    { width : Int
    , height : Int
    , scrollbarSize : ( Int, Int )
    , dialog : Dialog
    }


type Dialog
    = ArrowDialog LayerName Int Int
    | NoDialog


type alias Flags =
    { scrollbarSize : ( Int, Int )
    , now : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        _ =
            Debug.log "flags" flags
    in
    ( { width = 10
      , height = 8
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
    | ShowArrowDialog Prog.LayerName Int Int
    | DialogBackgroundClicked
    | GotScrollbarSize Int2


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

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


view : Model -> Html Msg
view model =
    E.layout
        [ E.inFront (viewDialog model)
        , E.height E.fill
        ]
        (E.column
            [ E.width E.fill
            ]
            [ viewProg ]
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
                        |> E.el [ E.centerX ]
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
