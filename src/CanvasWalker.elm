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


viewCell =
    div
        [ Style.widthPx gridConfig.cellSize
        , Style.heightPx gridConfig.cellSize
        , Style.bgColor "yellow"
        ]
        []


gridConfig =
    { rowCount = 30
    , colCount = 30
    , cellSize = 10
    }


viewGridRow =
    hStack [] (List.repeat gridConfig.colCount viewCell)


viewGrid =
    vStack []
        (List.repeat gridConfig.rowCount viewGridRow)


view : Model -> Html Msg
view { count, width, height } =
    vStack
        [ Class.pFixed
        , Class.trblZero
        ]
        [ viewGrid
        ]
