module GameOfLifeSvg3 exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Dict exposing (Dict)
import Html exposing (Html, div, text)
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


type alias CellData =
    ( Cell, Int )



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


cellDataGenerator : Int -> Generator (Array Cell)
cellDataGenerator length =
    Random.list length (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
        |> Random.map Array.fromList



--    [ Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
--        |> Array.fromList
--        |> Random.constant


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
        ++ [ ( -1, 0 ), {- ignore self (0,0) , -} ( 1, 0 ) ]
        ++ [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]



{-
   neighbourOffsets : List ( Int, Int )
   neighbourOffsets =
       [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
           ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
           ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]

-}


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
                                Just (ct + 1)
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
    cellDataGenerator grid.length
        |> Random.map updateGridFromCellArray


computeNextGrid grid =
    grid


main =
    let
        ( grid, _ ) =
            Random.step (gridGenerator 4 4) (Random.initialSeed 4)

        nextGrid =
            computeNextGrid grid
    in
    div []
        [ text (Debug.toString grid)
        , viewGrid grid
        ]


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
                        in
                        SL.lazy4 viewCell cellWidthInPx x y cell
                    )
                |> Array.toList
            )
        ]


viewCell : Float -> Int -> Int -> Cell -> Svg msg
viewCell cellWidthInPx gridX gridY cell =
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
