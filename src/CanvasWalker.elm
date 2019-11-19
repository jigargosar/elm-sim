module CanvasWalker exposing (main)

import Array exposing (Array)
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
    , grid : Grid
    , width : Float
    , height : Float
    }


type alias Grid =
    Array (Array Cell)


emptyGrid : Grid
emptyGrid =
    Array.repeat gridConfig.colCount Off
        |> Array.repeat gridConfig.rowCount


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
                  , grid = emptyGrid
                  }
                , Task.perform GotViewport getViewport
                )
        , view = view
        , update =
            \message model ->
                case message of
                    Tick delta ->
                        ( updateCollectedDeltaBy delta model
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


updateCollectedDeltaBy : Float -> Model -> Model
updateCollectedDeltaBy dd model =
    { model | collectedDelta = model.collectedDelta + dd }


targetFrameInMilli =
    1000 / 60


step : Model -> Model
step model =
    if model.collectedDelta > targetFrameInMilli then
        onFrame (updateCollectedDeltaBy -targetFrameInMilli model)
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


view : Model -> Html Msg
view model =
    vStack
        [ Class.pFixed, Class.trblZero ]
        [ viewGrid gridConfig model.grid ]


type Cell
    = On
    | Off


type alias GridConfig =
    { rowCount : Int, colCount : Int, cellSize : Float }


gridConfig : GridConfig
gridConfig =
    { rowCount = 30
    , colCount = 30
    , cellSize = 20
    }


cellAtRC : Int -> Int -> Grid -> Cell
cellAtRC row col grid =
    Array.get row grid
        |> Maybe.andThen (Array.get col)
        |> Maybe.withDefault Off


viewGrid : GridConfig -> Grid -> Html msg
viewGrid config grid =
    let
        viewGridCell : Int -> Int -> Html msg
        viewGridCell rowNum colNum =
            let
                cell : Cell
                cell =
                    cellAtRC rowNum colNum grid
            in
            div
                [ Style.widthPx config.cellSize
                , Style.heightPx config.cellSize
                , Style.bgColor
                    (case cell of
                        Off ->
                            "yellow"

                        On ->
                            "red"
                    )
                , Style.noShrink
                , style "box-shadow"
                    "inset 0 0 0px 0.5px rgb(0,0,0), 0 0 0px 0.5px rgb(0,0,0)"
                ]
                []

        viewGridRow : Int -> Html msg
        viewGridRow rowNum =
            hStack [] (List.range 0 config.colCount |> List.map (viewGridCell rowNum))
    in
    vStack
        []
        (List.range 0 config.rowCount |> List.map viewGridRow)
