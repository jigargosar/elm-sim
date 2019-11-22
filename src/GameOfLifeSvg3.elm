module GameOfLifeSvg3 exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Dict exposing (Dict)
import Html exposing (Html, text)
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
    {- Random.list length (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
       |> Random.map Array.fromList
    -}
    [ Dead, Dead, Dead, Dead, Alive, Dead, Dead, Dead, Dead, Dead, Dead, Dead ]
        |> Array.fromList
        |> Random.constant


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
                            let
                                x =
                                    remainderBy grid.width i

                                y =
                                    i // grid.height
                            in
                            ( i + 1
                            , neighbourOffsets
                                |> List.foldl
                                    (\( dx, dy ) ->
                                        Dict.update ((y + dy |> modBy height) * height + (x + dx |> modBy width))
                                            (\aliveCt ->
                                                case aliveCt of
                                                    Nothing ->
                                                        Just 1

                                                    Just ct ->
                                                        Just (ct + 1)
                                            )
                                    )
                                    lookup
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


main =
    let
        grid =
            Random.step (gridGenerator 3 4) (Random.initialSeed 4)
    in
    text (Debug.toString grid)
