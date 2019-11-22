module GameOfLifeSvg2 exposing (main)

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


gridGenerator : Int -> Int -> Generator Grid
gridGenerator width height =
    let
        grid =
            initDeadGrid width height

        updateGridFromCellArray cellArray =
            let
                reducer cell lookup =
                    case cell of
                        Alive ->
                            lookup

                        Dead ->
                            lookup

                aliveNeighboursLookup =
                    Array.foldl reducer grid.aliveNeighboursLookup cellArray
            in
            { grid | cellState = cellArray, aliveNeighboursLookup = aliveNeighboursLookup }
    in
    cellDataGenerator grid.length
        |> Random.map updateGridFromCellArray
