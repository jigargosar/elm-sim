module CanvasWalker exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Class
import GridOfLife as GOL
import Html exposing (..)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Style
import UI exposing (..)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> onAnimationFrameDelta Tick
        }


type alias Flags =
    { now : Int }


type alias Model =
    { elapsed : Float
    , grid : GOL.Grid
    , previousGrids : List GOL.Grid
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { elapsed = 0
      , grid = GOL.initEmpty gridConfig
      , previousGrids = []
      , seed = Random.initialSeed flags.now
      }
        |> randomizeGrid
    , Cmd.none
    )


randomizeGrid : Model -> Model
randomizeGrid model =
    let
        ( grid, seed ) =
            Random.step (GOL.generator gridConfig) model.seed
    in
    { model | grid = grid, previousGrids = [], seed = seed }


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Tick delta ->
            ( mapElapsedBy delta model |> step
            , Cmd.none
            )


mapElapsedBy : Float -> Model -> Model
mapElapsedBy dd model =
    { model | elapsed = model.elapsed + dd }


updatesPerSecond =
    100


step : Model -> Model
step model =
    let
        targetFrameInMilli =
            1000 / updatesPerSecond
    in
    if model.elapsed > targetFrameInMilli then
        updateOnFrame (mapElapsedBy -targetFrameInMilli model)
            |> step

    else
        model


updateOnFrame : Model -> Model
updateOnFrame model =
    { model | grid = GOL.nextState model.grid }
        |> pushLastGridState model.grid
        |> randomizeGridIfReachedStableState


pushLastGridState : GOL.Grid -> Model -> Model
pushLastGridState lastGrid model =
    { model | previousGrids = lastGrid :: model.previousGrids |> List.take 6 }


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
    List.any (is model.grid) model.previousGrids


view : Model -> Html Msg
view model =
    vStack
        [ Class.pFixed, Class.trblZero ]
        [ viewGrid model.grid ]


type alias GridConfig =
    { rowCount : Int
    , colCount : Int
    }


gridConfig : GridConfig
gridConfig =
    { rowCount = 30
    , colCount = 30
    }


viewGrid : GOL.Grid -> Html msg
viewGrid grid =
    let
        viewGridRow : List GOL.Cell -> Html msg
        viewGridRow cellRow =
            hStack [] (List.map viewCell cellRow)
    in
    vStack
        []
        (GOL.asList2d grid |> List.map viewGridRow)


viewCell : GOL.Cell -> Html msg
viewCell cell =
    let
        cellSize =
            20
    in
    div
        [ Style.widthPx cellSize
        , Style.heightPx cellSize
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
