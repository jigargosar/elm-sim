module CanvasWalker exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Class
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
    , grid : Grid
    , width : Float
    , height : Float
    , seed : Seed
    }


type Cell
    = On
    | Off


type alias GridRow =
    Array Cell


type alias Grid =
    Array GridRow


cellAtRC : Int -> Int -> Grid -> Cell
cellAtRC row col grid =
    Array.get row grid
        |> Maybe.andThen (Array.get col)
        |> Maybe.withDefault Off


neighbours : List ( Int, Int )
neighbours =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


neighboursOfRC : Int -> Int -> Grid -> List Cell
neighboursOfRC row col grid =
    neighbours
        |> List.map (\( nr, nc ) -> cellAtRC (row + nr) (col + nc) grid)


is =
    (==)


aliveNeighbourCountOfCellAtRC : Int -> Int -> Grid -> Int
aliveNeighbourCountOfCellAtRC row col grid =
    neighboursOfRC row col grid
        |> List.filter (is On)
        |> List.length


nextStateOfCellAtRC row col grid =
    let
        aliveNeighbourCount =
            aliveNeighbourCountOfCellAtRC row col grid
    in
    case cellAtRC row col grid of
        On ->
            if aliveNeighbourCount < 2 || aliveNeighbourCount > 3 then
                Off

            else
                On

        Off ->
            if aliveNeighbourCount == 3 then
                On

            else
                Off


emptyGrid : Grid
emptyGrid =
    Array.repeat gridConfig.colCount Off
        |> Array.repeat gridConfig.rowCount


randomArray : Int -> Generator a -> Generator (Array a)
randomArray count =
    Random.list count >> Random.map Array.fromList


randomGrid : Generator Grid
randomGrid =
    let
        randomGridCell : Generator Cell
        randomGridCell =
            Random.weighted ( 80, Off ) [ ( 20, On ) ]

        randomGridRow : Generator (Array Cell)
        randomGridRow =
            randomArray gridConfig.colCount randomGridCell
    in
    randomArray gridConfig.rowCount randomGridRow


randomizeGrid : Model -> Model
randomizeGrid model =
    let
        ( grid, seed ) =
            Random.step randomGrid model.seed
    in
    { model | grid = grid, seed = seed }


type Msg
    = Tick Float
    | GotViewport Viewport
    | BrowserResized Int Int


main : Program Flags Model Msg
main =
    Browser.element
        { init =
            \flags ->
                ( { collectedDelta = 0
                  , width = 400
                  , height = 400
                  , grid = emptyGrid
                  , seed = Random.initialSeed flags.now
                  }
                    |> randomizeGrid
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
    model


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
