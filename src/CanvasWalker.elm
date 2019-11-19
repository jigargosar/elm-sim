module CanvasWalker exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Class
import GridOfLife as GOL
import Html exposing (..)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Style
import Task
import UI exposing (..)


type alias Flags =
    { now : Int }


type alias Model =
    { collectedDelta : Float
    , grid : GOL.Grid
    , lastGridStates : List GOL.Grid
    , width : Float
    , height : Float
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { collectedDelta = 0
      , width = 400
      , height = 400
      , grid = GOL.emptyGrid gridConfig
      , lastGridStates = []
      , seed = Random.initialSeed flags.now
      }
        |> randomizeGrid
    , Task.perform GotViewport getViewport
    )


randomizeGrid : Model -> Model
randomizeGrid model =
    let
        ( grid, seed ) =
            Random.step (GOL.randomGrid gridConfig) model.seed
    in
    { model | grid = grid, seed = seed }


type Msg
    = Tick Float
    | GotViewport Viewport
    | BrowserResized Int Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
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


updatesPerSecond =
    10


step : Model -> Model
step model =
    let
        targetFrameInMilli =
            1000 / updatesPerSecond
    in
    if model.collectedDelta > targetFrameInMilli then
        updateOnFrame (updateCollectedDeltaBy -targetFrameInMilli model)
            |> step

    else
        model


updateOnFrame : Model -> Model
updateOnFrame model =
    { model | grid = GOL.nextGridState model.grid }
        |> pushLastGridState model.grid


pushLastGridState : GOL.Grid -> Model -> Model
pushLastGridState lastGrid model =
    { model | lastGridStates = lastGrid :: model.lastGridStates |> List.take 2 }


view : Model -> Html Msg
view model =
    vStack
        [ Class.pFixed, Class.trblZero ]
        [ viewGrid gridConfig model.grid ]


type alias GridConfig =
    { rowCount : Int, colCount : Int, cellSize : Float }


gridConfig : GridConfig
gridConfig =
    { rowCount = 30
    , colCount = 30
    , cellSize = 20
    }


viewGrid : GridConfig -> GOL.Grid -> Html msg
viewGrid config grid =
    let
        viewGridCell : Int -> Int -> Html msg
        viewGridCell rowNum colNum =
            let
                cell : GOL.Cell
                cell =
                    GOL.cellAtRC rowNum colNum grid
            in
            div
                [ Style.widthPx config.cellSize
                , Style.heightPx config.cellSize
                , Style.bgColor
                    (case cell of
                        GOL.Off ->
                            "yellow"

                        GOL.On ->
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
