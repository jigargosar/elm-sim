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
    { delta : Float
    , width : Float
    , height : Float
    }


type Msg
    = Tick Float
    | GotViewport Viewport


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( { delta = 0
                  , width = 400
                  , height = 400
                  }
                , Task.perform GotViewport getViewport
                )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Tick delta ->
                        ( addDelta delta model
                            |> step
                        , Cmd.none
                        )

                    GotViewport { viewport } ->
                        ( { model
                            | width = viewport.width
                            , height = viewport.height
                          }
                        , Cmd.none
                        )
        , subscriptions = \_ -> onAnimationFrameDelta Tick
        }


addDelta : number -> { a | delta : number } -> { a | delta : number }
addDelta delta model =
    { model | delta = model.delta + delta }


targetFrameInMilli =
    1000 / 60


step : Model -> Model
step model =
    if model.delta > targetFrameInMilli then
        onFrame { model | delta = model.delta - targetFrameInMilli }
            |> step

    else
        model


onFrame : Model -> Model
onFrame model =
    model


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
        viewGridCell : Html msg
        viewGridCell =
            div
                [ Style.widthPx c.cellSize
                , Style.heightPx c.cellSize
                , Style.bgColor "yellow"
                , Style.noShrink
                , style "box-shadow"
                    "inset 0 0 0px 0.5px rgb(0,0,0), 0 0 0px 0.5px rgb(0,0,0)"
                ]
                []

        viewGridRow : Html msg
        viewGridRow =
            hStack [] (List.repeat c.colCount viewGridCell)
    in
    vStack
        []
        (List.repeat c.rowCount viewGridRow)


view : Model -> Html Msg
view _ =
    vStack
        [ Class.pFixed, Class.trblZero ]
        [ viewGrid gridConfig ]
