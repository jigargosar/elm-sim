module GameOfLifeSvg3 exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Random exposing (Generator, Seed)
import Svg.Lazy as SL
import Time exposing (Posix)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST



-- CELL


type Cell
    = Alive
    | Dead


cellGenerator : Int -> Generator (Array Cell)
cellGenerator length =
    Random.list length (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
        |> Random.map Array.fromList


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



-- Grid


type alias Grid =
    { width : Int
    , height : Int
    , length : Int
    , cellState : Array Cell
    , aliveNeighboursLookup : Dict Int Int
    }


initDeadGrid : Int -> Int -> Grid
initDeadGrid width height =
    let
        length =
            width * height
    in
    Grid width height length (Array.repeat length Dead) Dict.empty


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
        ++ [ ( -1, 0 ), {- ignore self (0,0) , -} ( 1, 0 ) ]
        ++ [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]


gridIndexToXY : Int -> Grid -> ( Int, Int )
gridIndexToXY i grid =
    let
        x =
            remainderBy grid.width i

        y =
            i // grid.height
    in
    ( x, y )


incrementANC : Int -> Grid -> Dict Int Int -> Dict Int Int
incrementANC i grid lookup =
    let
        ( x, y ) =
            gridIndexToXY i grid

        { width, height } =
            grid
    in
    neighbourOffsets
        |> List.foldl
            (\( dx, dy ) ->
                Dict.update
                    ((y + dy |> modBy height) * height + (x + dx |> modBy width))
                    (\aliveCt ->
                        case aliveCt of
                            Nothing ->
                                Just 1

                            Just ct ->
                                Just (ct + 1)
                    )
            )
            lookup


decrementANC : Int -> Grid -> Dict Int Int -> Dict Int Int
decrementANC i grid lookup =
    let
        ( x, y ) =
            gridIndexToXY i grid

        { width, height } =
            grid
    in
    neighbourOffsets
        |> List.foldl
            (\( dx, dy ) ->
                Dict.update
                    ((y + dy |> modBy height) * height + (x + dx |> modBy width))
                    (\aliveCt ->
                        case aliveCt of
                            Nothing ->
                                Debug.todo "This should never happen"

                            Just 1 ->
                                Nothing

                            Just ct ->
                                Just (ct - 1)
                    )
            )
            lookup


gridGenerator : Int -> Int -> Generator Grid
gridGenerator width height =
    let
        grid =
            initDeadGrid width height

        updateGridFromCellArray cellArray =
            let
                reducer cell ( i, lookup ) =
                    case cell of
                        Alive ->
                            ( i + 1
                            , incrementANC i grid lookup
                            )

                        Dead ->
                            ( i + 1, lookup )

                ( _, aliveNeighboursLookup ) =
                    Array.foldl reducer ( 0, Dict.empty ) cellArray
            in
            { grid | cellState = cellArray, aliveNeighboursLookup = aliveNeighboursLookup }
    in
    cellGenerator grid.length
        |> Random.map updateGridFromCellArray


randomGridGeneratorFromGrid : Grid -> Generator Grid
randomGridGeneratorFromGrid grid =
    gridGenerator grid.width grid.height


computeNextGrid : Grid -> Grid
computeNextGrid grid =
    let
        getNextCellState : Int -> Cell -> Cell
        getNextCellState i =
            nextCellStateWithAliveNeighbourCount
                (Dict.get i grid.aliveNeighboursLookup |> Maybe.withDefault 0)

        newCellArray =
            Array.indexedMap getNextCellState grid.cellState

        reducer prevCell ( i, lookup ) =
            case ( prevCell, Array.get i newCellArray ) of
                ( _, Nothing ) ->
                    Debug.todo "This should never happen"

                ( Alive, Just Dead ) ->
                    ( i + 1, decrementANC i grid lookup )

                ( Dead, Just Alive ) ->
                    ( i + 1, incrementANC i grid lookup )

                _ ->
                    ( i + 1, lookup )

        ( _, newANL ) =
            Array.foldl reducer ( 0, grid.aliveNeighboursLookup ) grid.cellState
    in
    { grid | cellState = newCellArray, aliveNeighboursLookup = newANL }



-- MAIN


type alias Flags =
    { now : Int }


type alias Model =
    { grid : Grid
    , gridHistory : List Grid
    , seed : Seed
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        gridLen =
            60

        model : Model
        model =
            { grid = initDeadGrid gridLen gridLen
            , gridHistory = []
            , seed = Random.initialSeed 0
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


type Msg
    = Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( updateGridState model
            , Cmd.none
            )


randomizeGrid : Model -> Model
randomizeGrid model =
    model
        |> randomStep (randomGridGeneratorFromGrid model.grid)
        |> setGridFromTuple


updateGridState : Model -> Model
updateGridState model =
    mapGrid computeNextGrid model
        |> pushGridHistory model.grid
        |> randomizeGridIfFoundInHistory


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


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Browser.Events.onAnimationFrame Tick
        , update = update
        , view = .grid >> viewGrid
        }


viewGrid : Grid -> Html msg
viewGrid grid =
    let
        w =
            602

        h =
            w

        gridWidthInPx =
            w - 2

        cellWidthInPx =
            toFloat gridWidthInPx / toFloat grid.width
    in
    S.svg [ SA.viewBox 0 0 w h, HA.width w, HA.height h ]
        [ S.g
            [ SA.stroke Color.black
            , SA.strokeWidth (ST.px 1)
            ]
            (grid.cellState
                |> Array.indexedMap
                    (\i cell ->
                        let
                            ( x, y ) =
                                gridIndexToXY i grid

                            anc =
                                Dict.get i grid.aliveNeighboursLookup
                                    |> Maybe.withDefault 0
                                    |> String.fromInt
                        in
                        SL.lazy5 viewCell cellWidthInPx x y cell anc
                    )
                |> Array.toList
            )
        ]


viewCell : Float -> Int -> Int -> Cell -> String -> Svg msg
viewCell cellWidthInPx gridX gridY cell anc =
    let
        x =
            toFloat gridX * cellWidthInPx + 1

        y =
            toFloat gridY * cellWidthInPx + 1
    in
    S.g []
        [ S.rect
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

        {- , S.text_
           [ SA.x (ST.px x)
           , SA.y (ST.px <| y + 15)
           , SA.width (ST.px cellWidthInPx)
           , SA.height (ST.px cellWidthInPx)
           ]
           [ SC.text anc ]
        -}
        ]
