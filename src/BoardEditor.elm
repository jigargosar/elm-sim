port module BoardEditor exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import CD
import Element as E exposing (centerX, centerY, column, el, fill, height, inFront, none, padding, paddingXY, row, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import Inst
import Prog exposing (LayerName, Prog)
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
    , prog : Prog
    }


type Dialog
    = ArrowDialog LayerName Int Int
    | NoDialog


type alias Flags =
    { scrollbarSize : ( Int, Int )
    , now : Int
    }


initialProg : Prog
initialProg =
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
      , prog = initialProg
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
            [ viewProg model.prog ]
        )


viewDialog model =
    case model.dialog of
        NoDialog ->
            none

        ArrowDialog layerName x y ->
            dialogContainer model.scrollbarSize (viewArrowDialogContent layerName x y)


dialogContainer ( scrollbarXWidth, _ ) content =
    el
        [ width fill
        , height fill
        , Background.color (E.rgba 0 0 0 0.5)
        , inFront
            (el
                [ centerX
                , centerY
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
        (el [ width fill, height fill, onClick DialogBackgroundClicked ] none)


viewArrowDialogContent layerName x y =
    column
        [ padding 10
        , width (E.shrink |> E.minimum 200)
        , height (E.shrink |> E.minimum 200)
        ]
        [ E.text "ArrowDialog"
        ]


viewProg prog =
    renderGrid
        [ Border.width 1
        , Border.color lightGray
        , E.centerX
        , E.width E.shrink
        ]
        10
        8
        (viewProgCell prog)


viewProgCell prog x y =
    row
        [ Border.width 1
        , Border.color lightGray
        , E.padding 5
        , E.width (E.minimum 30 E.fill |> E.maximum 100)
        , E.height (E.minimum 30 E.fill |> E.maximum 100)
        , E.spacing 10
        , E.scrollbars
        ]
        [ layerCellColumn Prog.red x y prog
        , layerCellColumn Prog.blue x y prog
        ]


layerCellColumn layerName x y prog =
    let
        color =
            if layerName == Prog.red then
                red

            else
                blue
    in
    E.column
        [ Font.color color
        , E.width E.fill
        , spacing 5
        , E.alignTop
        ]
        [ E.text
            (Prog.instAt layerName x y prog
                |> Maybe.map Debug.toString
                |> Maybe.withDefault " "
            )
            |> E.el [ E.centerX ]
        , E.text
            (Prog.arrowAt layerName x y prog
                |> Maybe.map CD.arrowSymbol
                |> Maybe.withDefault " "
            )
            |> E.el [ E.centerX, Events.onClick (ShowArrowDialog layerName x y) ]
        ]


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
