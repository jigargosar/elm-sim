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
    , cellSize = 10
    }


viewGrid : GridConfig -> Html msg
viewGrid c =
    let
        borderSize =
            1

        outlineSize =
            0

        viewGridCell : Html msg
        viewGridCell =
            div
                [ Style.widthPx c.cellSize
                , Style.heightPx c.cellSize
                , Style.bgColor "yellow"
                , Style.border [ Style.px borderSize, "solid", "rgba(0,0,0,1)" ]
                , Style.outline [ Style.px outlineSize, "solid", "rgba(0,0,0,1)" ]
                , Style.noShrink
                ]
                []

        viewGridRow : Html msg
        viewGridRow =
            hStack [] (List.repeat c.colCount viewGridCell)
    in
    vStack [ Style.transform [ "scale(2)" ] ]
        (List.repeat c.rowCount viewGridRow)


view : Model -> Html Msg
view _ =
    vStack
        [ Class.pFixed, Class.trblZero ]
        [ viewGrid gridConfig ]
