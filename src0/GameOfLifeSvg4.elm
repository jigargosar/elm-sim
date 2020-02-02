module GameOfLifeSvg4 exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as HA
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg.Lazy as SL
import Time exposing (Posix)
import TypedSvg as S
import TypedSvg.Attributes as SA
import View


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


nextCellStateWithANC : Int -> Bool -> Bool
nextCellStateWithANC anc cell =
    if cell then
        not (anc < 2 || anc > 3)

    else
        anc == 3



-- GRID


type alias Pos =
    ( Int, Int )


type alias Grid =
    { width : Int
    , height : Int
    , length : Int
    , cords : List Pos
    , data : Set Pos
    }


toCords : Int -> Int -> List Pos
toCords w h =
    let
        widthRange : List Int
        widthRange =
            List.range 0 (w - 1)

        heightRange : List Int
        heightRange =
            List.range 0 (h - 1)
    in
    List.concatMap (\y -> List.map (\x -> ( x, y )) widthRange) heightRange


initialGrid : Int -> Int -> Grid
initialGrid width height =
    let
        length =
            width * height

        cords =
            toCords width height
    in
    Grid width height length cords Set.empty


randomGridGeneratorFromGrid : Grid -> Generator Grid
randomGridGeneratorFromGrid grid =
    let
        cellGen : Generator Bool
        cellGen =
            Random.weighted ( 10, True ) [ ( 90, False ) ]

        cellListGen : Generator (List Bool)
        cellListGen =
            Random.list grid.length cellGen

        dataGenerator : Generator (Set Pos)
        dataGenerator =
            cellListGen
                |> Random.map
                    (\cellList ->
                        List.map2
                            (\cord aliveBool ->
                                if aliveBool then
                                    Just cord

                                else
                                    Nothing
                            )
                            grid.cords
                            cellList
                            |> List.filterMap identity
                            |> Set.fromList
                    )
    in
    dataGenerator |> Random.map (\data -> { grid | data = data })


ancOfPos : Pos -> Grid -> Int
ancOfPos ( x, y ) grid =
    let
        w =
            grid.width

        h =
            grid.height
    in
    List.foldl
        (\( dx, dy ) ct ->
            if Set.member ( x + dx |> modBy w, y + dy |> modBy h ) grid.data then
                ct + 1

            else
                ct
        )
        0
        neighbourOffsets


nextGridState : Grid -> Grid
nextGridState grid =
    { grid | data = List.foldl (\data -> nextGridDataHelp grid data) grid.data grid.cords }


nextGridDataHelp : Grid -> Pos -> Set Pos -> Set Pos
nextGridDataHelp grid p data =
    let
        prevCell : Bool
        prevCell =
            Set.member p grid.data

        anc =
            ancOfPos p grid

        nextCell : Bool
        nextCell =
            nextCellStateWithANC anc prevCell
    in
    if prevCell == nextCell then
        data

    else if nextCell then
        Set.insert p data

    else
        Set.remove p data



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
        [ S.g [{- SA.stroke Color.black , SA.strokeWidth (ST.px 1) -}]
            (List.map
                (\(( x, y ) as p) ->
                    SL.lazy4 View.cell
                        cellWidthInPx
                        x
                        y
                        (Set.member p grid.data)
                )
                grid.cords
            )
        ]



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
