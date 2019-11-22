module GameOfLifeSvg2 exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Html.Attributes as HA
import Random exposing (Generator, Seed)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Types as ST



-- CELL


type Cell
    = Alive
    | Dead


nextCellState : Int -> Int -> (Int -> Int -> Maybe Cell) -> Cell -> Cell
nextCellState i j cellAt =
    getAliveNeighbourCount i j cellAt
        |> nextCellStateWithAliveNeighbourCount


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


getAliveNeighbourCount : Int -> Int -> (Int -> Int -> Maybe Cell) -> Int
getAliveNeighbourCount i j cellAt =
    neighbourOffsets
        |> List.foldl
            (\( di, dj ) ct ->
                case cellAt (i + di) (j + dj) of
                    Just Alive ->
                        ct + 1

                    _ ->
                        ct
            )
            0


{-| <https://www.conwaylife.com/wiki/Conway's_Game_of_Life>
-}
nextCellStateWithAliveNeighbourCount : Int -> Cell -> Cell
nextCellStateWithAliveNeighbourCount aliveNeighbourCount cell =
    case cell of
        Alive ->
            {- Any live cell with fewer than two live neighbours dies
                (referred to as underpopulation or exposure[1]).
               Any live cell with more than three live neighbours dies
                (referred to as overpopulation or overcrowding).
            -}
            if aliveNeighbourCount < 2 || aliveNeighbourCount > 3 then
                Dead

            else
                {- Any live cell with two or three live neighbours lives,
                   unchanged, to the next generation.
                -}
                Alive

        Dead ->
            if aliveNeighbourCount == 3 then
                Alive

            else
                Dead



-- GRID


type alias Grid =
    { width : Int
    , height : Int
    , data : Array Cell
    }


initialGrid : Int -> Int -> Grid
initialGrid width height =
    Array.repeat (width * height) Dead
        |> Grid width height


randomGridGenerator : Int -> Int -> Generator Grid
randomGridGenerator width height =
    Random.list (width * height) (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
        |> Random.map (Array.fromList >> Grid width height)


randomGridGeneratorFromGrid : Grid -> Generator Grid
randomGridGeneratorFromGrid grid =
    randomGridGenerator grid.width grid.height


gridIndexToXY : Int -> Grid -> ( Int, Int )
gridIndexToXY i grid =
    let
        x =
            remainderBy grid.width i

        y =
            i // grid.height
    in
    ( x, y )


gridIndexFromXY : Int -> Int -> Grid -> Int
gridIndexFromXY x y grid =
    modBy grid.width x + modBy grid.height y * grid.height


getGridCellAt : Int -> Int -> Grid -> Maybe Cell
getGridCellAt x y grid =
    let
        i =
            gridIndexFromXY x y grid
    in
    Array.get i grid.data


nextGridState : Grid -> Grid
nextGridState grid =
    let
        cellAt x y =
            getGridCellAt x y grid
    in
    Array.indexedMap
        (\i ->
            let
                ( x, y ) =
                    gridIndexToXY i grid
            in
            nextCellState x y cellAt
        )
        grid.data
        |> Grid grid.width grid.height



-- Model


type alias Flags =
    { now : Int }


type alias Model =
    { grid : Grid
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        model =
            { grid = initialGrid 30 30
            , seed = Random.initialSeed now
            }
    in
    ( randomizeGrid model
    , Cmd.none
    )


setGridFromTuple : ( Grid, Model ) -> Model
setGridFromTuple ( grid, model ) =
    { model | grid = grid }


randomStep : Generator a -> Model -> ( a, Model )
randomStep generator model =
    Random.step generator model.seed
        |> Tuple.mapSecond (\seed -> { model | seed = seed })



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Tick _ ->
            ( { model | grid = nextGridState model.grid }
            , Cmd.none
            )


randomizeGrid : Model -> Model
randomizeGrid model =
    model
        |> randomStep (randomGridGeneratorFromGrid model.grid)
        |> setGridFromTuple



-- VIEW


view : Model -> Html Msg
view model =
    let
        w =
            600

        h =
            w

        gridWidthInPx =
            w - 2

        grid =
            model.grid

        cellWidthInPx =
            toFloat gridWidthInPx / toFloat grid.width

        viewCell ( gridX, gridY ) cell =
            let
                x =
                    toFloat gridX * cellWidthInPx + 1

                y =
                    toFloat gridY * cellWidthInPx + 1
            in
            S.rect
                [ (if cell == Dead then
                    Color.lightYellow

                   else
                    Color.lightRed
                  )
                    |> ST.Fill
                    |> SA.fill
                , SA.x (ST.px x)
                , SA.y (ST.px y)
                , SA.width (ST.px cellWidthInPx)
                , SA.height (ST.px cellWidthInPx)
                ]
                []
    in
    S.svg [ SA.viewBox 0 0 w h, HA.width w, HA.height h ]
        [ S.g
            [ SA.stroke Color.black
            , SA.strokeWidth (ST.px 2)
            ]
            (grid.data
                |> Array.indexedMap (\i -> viewCell (gridIndexToXY i grid))
                |> Array.toList
            )
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta Tick
        , update = update
        , view = view
        }
