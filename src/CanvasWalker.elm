module CanvasWalker exposing (main)

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
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { collectedDelta = 0
      , grid = GOL.emptyGrid gridConfig
      , lastGridStates = []
      , seed = Random.initialSeed flags.now
      }
        |> randomizeGrid
    , Cmd.none
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
        , subscriptions =
            \_ ->
                Sub.batch
                    [ onAnimationFrameDelta Tick
                    ]
        }


updateCollectedDeltaBy : Float -> Model -> Model
updateCollectedDeltaBy dd model =
    { model | collectedDelta = model.collectedDelta + dd }


updatesPerSecond =
    60


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
        |> randomizeGridIfReachedStableState


pushLastGridState : GOL.Grid -> Model -> Model
pushLastGridState lastGrid model =
    { model | lastGridStates = lastGrid :: model.lastGridStates |> List.take 2 }


randomizeGridIfReachedStableState : Model -> Model
randomizeGridIfReachedStableState model =
    if isGridStable model then
        randomizeGrid model

    else
        model


is =
    (==)


isGridStable : Model -> Bool
isGridStable model =
    List.any (is model.grid) model.lastGridStates


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
