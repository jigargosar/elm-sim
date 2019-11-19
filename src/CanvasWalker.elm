module CanvasWalker exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Class
import Html exposing (..)
import Html.Attributes exposing (style)
import Style
import Task
import UI exposing (..)


type alias Model =
    { count : Float, width : Float, height : Float }


type Msg
    = Tick Float
    | GotViewport Viewport


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( { count = 0
                  , width = 400
                  , height = 400
                  }
                , Task.perform GotViewport getViewport
                )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Tick _ ->
                        ( { model | count = model.count + 1 }, Cmd.none )

                    GotViewport { viewport } ->
                        ( { model
                            | width = viewport.width
                            , height = viewport.height
                          }
                        , Cmd.none
                        )
        , subscriptions = \_ -> onAnimationFrameDelta Tick
        }


type alias GridConfig =
    { rowCount : Int, colCount : Int, cellSize : Float }


gridConfig : GridConfig
gridConfig =
    { rowCount = 30
    , colCount = 30
    , cellSize = 20
    }


viewGrid : GridConfig -> Html msg
viewGrid c =
    let
        borderArgs =
            [ Style.px 1, "solid", "rgba(0,0,0,1)" ]

        outlineSize =
            0

        viewGridCell : Html msg
        viewGridCell =
            div
                [ Style.widthPx c.cellSize
                , Style.heightPx c.cellSize
                , Style.bgColor "yellow"
                , Style.borderTop borderArgs
                , Style.borderLeft borderArgs
                , Style.outline [ Style.px outlineSize, "solid", "rgba(0,0,0,1)" ]
                , Style.noShrink
                ]
                []

        viewGridRow : Html msg
        viewGridRow =
            hStack [] (List.repeat c.colCount viewGridCell)
    in
    hStack []
        [ vStack
            [ Style.borderBottom borderArgs
            , Style.borderRight borderArgs
            ]
            (List.repeat c.rowCount viewGridRow)
        ]


view : Model -> Html Msg
view _ =
    vStack
        [ Class.pFixed, Class.trblZero ]
        [ viewGrid gridConfig ]
