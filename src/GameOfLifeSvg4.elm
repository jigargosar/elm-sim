module GameOfLifeSvg4 exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Random exposing (Generator, Seed)
import Svg.Lazy as SL
import Time exposing (Posix)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import View



-- CELL


type Cell
    = Alive


isAlive : Maybe Cell -> Bool
isAlive =
    (/=) Nothing


aliveState =
    Just Alive


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


{-| <https://www.conwaylife.com/wiki/Conway's_Game_of_Life>
-}
nextCellStateWithANC : Int -> Maybe Cell -> Maybe Cell
nextCellStateWithANC aliveNeighbourCount cell =
    case cell of
        Just Alive ->
            {- Any live cell with fewer than two live neighbours dies
                (referred to as underpopulation or exposure[1]).
               Any live cell with more than three live neighbours dies
                (referred to as overpopulation or overcrowding).
            -}
            if aliveNeighbourCount < 2 || aliveNeighbourCount > 3 then
                Nothing

            else
                {- Any live cell with two or three live neighbours lives,
                   unchanged, to the next generation.
                -}
                aliveState

        Nothing ->
            if aliveNeighbourCount == 3 then
                aliveState

            else
                Nothing



-- GRID


type alias Pos =
    ( Int, Int )


type alias Grid =
    { width : Int
    , height : Int
    , length : Int
    , cords : List Pos
    , neighboursCordsLookup : Dict Pos (List Pos)
    , data : Dict Pos Cell
    }


toCords : Int -> Int -> List Pos
toCords w h =
    let
        widthRange =
            List.range 0 (w - 1)

        heightRange =
            List.range 0 (h - 1)
    in
    List.concatMap (\y -> List.map (\x -> ( x, y )) widthRange) heightRange


cordsToNeighboursPosLookup : Int -> Int -> List Pos -> Dict Pos (List Pos)
cordsToNeighboursPosLookup w h =
    let
        toNCord ( x, y ) =
            neighbourOffsets
                |> List.map (\( dx, dy ) -> ( x + dx |> modBy w, y + dy |> modBy h ))
    in
    List.map (\pos -> ( pos, toNCord pos ))
        >> Dict.fromList


initialGrid : Int -> Int -> Grid
initialGrid width height =
    let
        length =
            width * height

        cords =
            toCords width height
    in
    Grid width height length cords (cordsToNeighboursPosLookup width height cords) Dict.empty


randomGridGeneratorFromGrid : Grid -> Generator Grid
randomGridGeneratorFromGrid grid =
    let
        cellGen : Generator (Maybe Cell)
        cellGen =
            Random.weighted ( 10, aliveState ) [ ( 90, Nothing ) ]

        cellListGen : Generator (List (Maybe Cell))
        cellListGen =
            Random.list grid.length cellGen

        dataGenerator : Generator (Dict Pos Cell)
        dataGenerator =
            cellListGen
                |> Random.map
                    (\cellList ->
                        List.map2 (\cord -> Maybe.map (Tuple.pair cord)) grid.cords cellList
                            |> List.filterMap identity
                            |> Dict.fromList
                    )
    in
    dataGenerator |> Random.map (\data -> { grid | data = data })


ancOfPos : Pos -> Grid -> Int
ancOfPos p grid =
    case Dict.get p grid.neighboursCordsLookup of
        Nothing ->
            0

        Just neighbourPosList ->
            neighbourPosList
                |> List.foldl
                    (\np ct ->
                        case Dict.get np grid.data of
                            Just Alive ->
                                ct + 1

                            _ ->
                                ct
                    )
                    0


nextGridState : Grid -> Grid
nextGridState grid =
    { grid | data = List.foldl (nextGridDataHelp grid) grid.data grid.cords }


nextGridDataHelp : Grid -> Pos -> Dict Pos Cell -> Dict Pos Cell
nextGridDataHelp grid p =
    let
        prevCell =
            Dict.get p grid.data

        anc =
            ancOfPos p grid

        nextCell =
            nextCellStateWithANC anc prevCell
    in
    if prevCell == nextCell then
        identity

    else
        case nextCell of
            Just Alive ->
                Dict.insert p Alive

            Nothing ->
                Dict.remove p



-- Model


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
            120

        model : Model
        model =
            { grid = initialGrid gridLen gridLen
            , gridHistory = []
            , seed = Random.initialSeed now
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
    mapGrid nextGridState model
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
            toFloat gridWidthInPx / toFloat grid.width
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
                        SL.lazy4 viewCell
                            cellWidthInPx
                            x
                            y
                            (Dict.get ( x, y ) grid.data)
                    )
            )
        ]


viewCell : Float -> Int -> Int -> Maybe Cell -> Svg Msg
viewCell cellWidthInPx gridX gridY cell =
    View.cell cellWidthInPx gridX gridY (isAlive cell)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions _ =
    Browser.Events.onAnimationFrame Tick
