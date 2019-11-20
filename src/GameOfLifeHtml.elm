module GameOfLifeHtml exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onMouseUp)
import Class
import GridOfLife as GOL
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseOver)
import Json.Decode as JD
import Random exposing (Generator, Seed)
import Style
import UI exposing (..)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                onAnimationFrameDelta Tick
                    :: onMouseDown (JD.succeed (MouseDown True))
                    :: onMouseUp (JD.succeed (MouseDown False))
                    :: []
                    |> Sub.batch
        }


type alias Flags =
    { now : Int }


type alias Model =
    { elapsed : Float
    , isMouseDown : Bool
    , grid : GOL.Grid
    , previousGrids : List GOL.Grid
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { elapsed = 0
      , isMouseDown = False
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
            Random.step (GOL.randomize model.grid) model.seed
    in
    { model | grid = grid, previousGrids = [], seed = seed }


type Msg
    = Tick Float
    | CellClicked Int Int
    | MouseOverCell Int Int
    | MouseDown Bool


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Tick delta ->
            ( mapElapsedBy delta model |> step
            , Cmd.none
            )

        CellClicked rIdx cIdx ->
            ( mapGrid (GOL.toggleCellAtRC rIdx cIdx) model
            , Cmd.none
            )

        MouseOverCell ri ci ->
            ( if model.isMouseDown then
                mapGrid (GOL.toggleCellAtRC ri ci) model

              else
                model
            , Cmd.none
            )

        MouseDown bool ->
            ( { model | isMouseDown = bool }, Cmd.none )


mapGrid : (GOL.Grid -> GOL.Grid) -> Model -> Model
mapGrid func model =
    { model | grid = func model.grid }


mapElapsedBy : Float -> Model -> Model
mapElapsedBy dd model =
    { model | elapsed = model.elapsed + dd }


updatesPerSecond =
    2


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


is : a -> a -> Bool
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


viewGrid : GOL.Grid -> Html Msg
viewGrid grid =
    let
        viewGridRow : Int -> List GOL.Cell -> Html Msg
        viewGridRow rowIdx cellRow =
            hStack [] (List.indexedMap (viewCell rowIdx) cellRow)
    in
    vStack []
        (GOL.toListRC grid |> List.indexedMap viewGridRow)


viewCell : Int -> Int -> GOL.Cell -> Html Msg
viewCell rowIdx colIdx cell =
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
        , onClick (CellClicked rowIdx colIdx)
        , onMouseOver (MouseOverCell rowIdx colIdx)
        ]
        []
