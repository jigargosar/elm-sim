module GameOfLifeSvg5 exposing (main)

import Browser
import Browser.Events
import Color
import Dict exposing (Dict)
import GameOfLifeSvg5Grid as Grid
import Html exposing (Html)
import Html.Attributes as HA
import Random exposing (Generator, Seed)
import Svg.Lazy as SL
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST



{-
   Uses GameOfLifeSvg5Grid Implementation which stores and
   updates AliveNeighbourCount of neighbours
   of a cell being toggled.
-}


type alias Flags =
    { now : Int }


type alias Grid =
    Grid.Model


type alias Model =
    { grid : Grid
    , gridHistory : List Grid
    , seed : Seed
    , delta : Float
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        gridLen =
            120

        model : Model
        model =
            { grid = Grid.initDead gridLen gridLen
            , gridHistory = []
            , seed = Random.initialSeed now
            , delta = 0
            }
    in
    ( randomizeGrid model
    , Cmd.none
    )


setGridFromTuple : ( Grid, Model ) -> Model
setGridFromTuple ( grid, model ) =
    { model | grid = grid }


mapGrid : (Grid -> Grid) -> Model -> Model
mapGrid func model =
    { model | grid = func model.grid }


randomStep : Generator a -> Model -> ( a, Model )
randomStep generator model =
    Random.step generator model.seed
        |> Tuple.mapSecond (\seed -> { model | seed = seed })



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick delta ->
            ( updateGridState delta model
            , Cmd.none
            )


randomizeGrid : Model -> Model
randomizeGrid model =
    model
        |> randomStep (Grid.randomize model.grid)
        |> setGridFromTuple


updateGridState : Float -> Model -> Model
updateGridState delta model =
    {-
       let
           accDelta =
               model.delta + delta

           interval =
               1000 / 60
       in
       if accDelta > interval then
    -}
    model
        --|> setDelta (accDelta - interval)
        |> mapGrid Grid.nextState
        |> pushGridHistory model.grid
        |> randomizeGridIfFoundInHistory



{- else
   model
       |> setDelta accDelta
-}


setDelta delta model =
    { model | delta = delta }


pushGridHistory grid model =
    { model | gridHistory = grid :: model.gridHistory |> List.take 6 }


clearGridHistory model =
    { model | gridHistory = [] }


randomizeGridIfFoundInHistory model =
    if List.member model.grid model.gridHistory then
        randomizeGrid model
            |> clearGridHistory

    else
        model



-- VIEW


view : Model -> Html Msg
view model =
    viewGrid model.grid


viewGrid : Grid -> Html Msg
viewGrid grid =
    let
        w =
            602

        h =
            w

        gridWidthInPx =
            w - 2

        cellWidthInPx =
            toFloat gridWidthInPx / toFloat (Grid.width grid)
    in
    S.svg [ SA.viewBox 0 0 w h, HA.width w, HA.height h ]
        [ S.g
            [{- SA.stroke Color.black
                , SA.strokeWidth (ST.px 1)
             -}
            ]
            (grid.cords
                |> List.map
                    (\( x, y ) ->
                        let
                            cell =
                                Dict.get ( x, y ) grid.data |> Maybe.map Tuple.first
                        in
                        SL.lazy4 viewCell
                            cellWidthInPx
                            x
                            y
                            cell
                    )
            )
        ]


viewCell : Float -> Int -> Int -> Maybe Grid.Cell -> Svg Msg
viewCell cellWidthInPx gridX gridY cell =
    let
        x =
            toFloat gridX * cellWidthInPx + 1

        y =
            toFloat gridY * cellWidthInPx + 1
    in
    S.g []
        [ S.rect
            [ (if cell == Just Grid.Alive then
                Color.lightRed

               else
                Color.lightYellow
              )
                |> ST.Fill
                |> SA.fill
            , SA.x (ST.px x)
            , SA.y (ST.px y)
            , SA.width (ST.px cellWidthInPx)
            , SA.height (ST.px cellWidthInPx)
            ]
            []
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta Tick
        , update = update
        , view = view
        }
