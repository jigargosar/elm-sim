module GameOfLifeSvg exposing (main)

import Browser
import Browser.Events as B
import Color
import Html exposing (..)
import Html.Attributes as H
import Html.Lazy exposing (lazy)
import Json.Decode as JD
import MatrixOfLife as MOL
import Random exposing (Generator, Seed)
import Svg.Lazy as S
import TypedSvg as S exposing (svg)
import TypedSvg.Attributes as S
import TypedSvg.Core as S exposing (Svg)
import TypedSvg.Events as S
import TypedSvg.Types as S
import UI exposing (..)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                B.onAnimationFrameDelta Tick
                    :: B.onMouseUp (JD.succeed (MouseDown False))
                    :: B.onKeyDown (JD.field "key" JD.string |> JD.map KeyDown)
                    :: []
                    |> Sub.batch
        }


type alias Flags =
    { now : Int }


type alias Model =
    { elapsed : Float
    , isMouseDown : Bool
    , isPaused : Bool
    , grid : MOL.Grid
    , previousGrids : List MOL.Grid
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { elapsed = 0
      , isMouseDown = False
      , isPaused = False
      , grid = MOL.initEmpty gridConfig
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
            Random.step (MOL.randomize model.grid) model.seed
    in
    { model | grid = grid, previousGrids = [], seed = seed }


type Msg
    = Tick Float
    | MouseOverCell Int Int
    | MouseDown Bool
    | KeyDown String
    | MouseDownOnCell Int Int


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Tick delta ->
            ( mapElapsedBy delta model |> step
            , Cmd.none
            )

        KeyDown key ->
            ( case key of
                " " ->
                    togglePaused model

                _ ->
                    model
            , Cmd.none
            )

        MouseOverCell ri ci ->
            ( if model.isMouseDown then
                toggleCellRC ri ci model

              else
                model
            , Cmd.none
            )

        MouseDown bool ->
            ( setMouseDown bool model, Cmd.none )

        MouseDownOnCell ri ci ->
            ( model
                |> setMouseDown True
                |> toggleCellRC ri ci
            , Cmd.none
            )


setMouseDown : a -> { b | isMouseDown : a } -> { b | isMouseDown : a }
setMouseDown isMouseDown model =
    { model | isMouseDown = isMouseDown }


toggleCellRC : Int -> Int -> Model -> Model
toggleCellRC ri ci =
    mapGrid (MOL.toggleCellAtRC ri ci)


togglePaused : { a | isPaused : Bool } -> { a | isPaused : Bool }
togglePaused model =
    { model | isPaused = not model.isPaused }


mapGrid : (MOL.Grid -> MOL.Grid) -> Model -> Model
mapGrid func model =
    { model | grid = func model.grid }


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
    if model.isPaused then
        model

    else
        { model | grid = MOL.nextState model.grid }
            |> pushLastGridState model.grid
            |> randomizeGridIfReachedStableState


pushLastGridState : MOL.Grid -> Model -> Model
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
        []
        [ lazy viewGridSvg model.grid
        ]


viewGridSvg : MOL.Grid -> Html Msg
viewGridSvg grid =
    hStack []
        [ let
            s =
                600

            gridWidthInPx =
                s - 2

            cellSize =
                toFloat gridWidthInPx / toFloat gridConfig.rowCount
          in
          svg [ S.viewBox 0 0 s s, H.width s ]
            [ viewGridSvgGroup cellSize grid
            ]
        ]


viewGridSvgGroup : Float -> MOL.Grid -> Svg Msg
viewGridSvgGroup cellSize grid =
    S.g
        [ S.stroke Color.black
        , S.strokeWidth (S.px 2)
        ]
        (viewGridCellsSvg cellSize grid)


viewGridCellsSvg : Float -> MOL.Grid -> List (Svg Msg)
viewGridCellsSvg cellSize grid =
    MOL.indexedMapToList
        (S.lazy4 viewCellRCSvg cellSize)
        grid


viewCellRCSvg : Float -> Int -> Int -> MOL.Cell -> S.Svg Msg
viewCellRCSvg cellSize ri ci cell =
    let
        _ =
            -- Debug.log "rc" ( ri, ci )
            1
    in
    S.rect
        [ (case cell of
            MOL.Off ->
                Color.lightYellow

            MOL.On ->
                Color.lightRed
          )
            |> S.Fill
            |> S.fill
        , S.x (S.px <| toFloat ci * cellSize + 1)
        , S.y (S.px <| toFloat ri * cellSize + 1)
        , S.width (S.px cellSize)
        , S.height (S.px cellSize)
        , S.onMouseOver (MouseOverCell ri ci)
        , S.onMouseDown (MouseDownOnCell ri ci)
        ]
        []


type alias GridConfig =
    { rowCount : Int
    , colCount : Int
    }


gridConfig : GridConfig
gridConfig =
    { rowCount = 30
    , colCount = 30
    }
