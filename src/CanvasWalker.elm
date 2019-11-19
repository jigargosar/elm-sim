module CanvasWalker exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Class
import Html exposing (..)
import Html.Attributes exposing (style)
import Style
import Task
import UI exposing (..)


type alias Model =
    { collectedDelta : Float
    , width : Float
    , height : Float
    }


type Msg
    = Tick Float
    | GotViewport Viewport
    | BrowserResized Int Int


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( { collectedDelta = 0
                  , width = 400
                  , height = 400
                  }
                , Task.perform GotViewport getViewport
                )
        , view = view
        , update =
            \message model ->
                case message of
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

                    BrowserResized w h ->
                        ( { model | width = toFloat w, height = toFloat h }
                        , Cmd.none
                        )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Tick
                    , onResize BrowserResized
                    ]
        }


addDelta : Float -> Model -> Model
addDelta delta model =
    { model | collectedDelta = model.collectedDelta + delta }


targetFrameInMilli =
    1000 / 60


step : Model -> Model
step model =
    if model.collectedDelta > targetFrameInMilli then
        onFrame { model | collectedDelta = model.collectedDelta - targetFrameInMilli }
            |> step

    else
        model


onFrame : Model -> Model
onFrame model =
    let
        _ =
            Debug.log "onFrame" model
    in
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
