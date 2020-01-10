module Puzzle exposing (main)

-- MAIN

import Playground exposing (..)



-- MODEL


type alias Mem =
    { width : Int
    , height : Int
    }


init : Mem
init =
    { width = 10
    , height = 10
    }



-- UPDATE


update : Computer -> Mem -> Mem
update computer mem =
    mem



-- VIEW


view : Computer -> Mem -> List Shape
view { screen } mem =
    let
        cfg =
            toConfig screen mem
    in
    [ rectangle black cfg.width cfg.height
    , mapAllPos
        (\pos ->
            [ circle white (cfg.cellRadius * 0.9) ]
                |> group
                |> moveCell cfg pos
        )
        mem
        |> group
    ]



-- Main


main =
    game view update init



-- Common


type alias Pos =
    ( Int, Int )


type alias Config =
    { cellSize : Float
    , cellRadius : Float
    , toScreen : Pos -> ( Float, Float )
    , toGrid : ( Float, Float ) -> Pos
    , width : Float
    , height : Float
    }


toConfig : { a | width : Float, height : Float } -> { b | width : Int, height : Int } -> Config
toConfig screen mem =
    let
        cellSize =
            min (screen.width / (toFloat mem.width + 1))
                (screen.height / (toFloat mem.height + 2))

        dx =
            (cellSize - (cellSize * toFloat mem.width)) / 2

        dy =
            (cellSize - (cellSize * toFloat mem.height)) / 2

        toScreen ( gx, gy ) =
            ( (toFloat gx * cellSize) + dx, (toFloat gy * cellSize) + dy )

        toGrid ( x, y ) =
            ( round ((x - dx) / cellSize), round ((y - dy) / cellSize) )
    in
    { cellSize = cellSize
    , cellRadius = cellSize / 2
    , toScreen = toScreen
    , toGrid = toGrid
    , width = cellSize * toFloat mem.width
    , height = cellSize * toFloat mem.height
    }


moveCell : { a | toScreen : b -> ( Number, Number ) } -> b -> Shape -> Shape
moveCell cfg pos =
    move (cfg.toScreen pos)


move : ( Number, Number ) -> Shape -> Shape
move ( x, y ) =
    Playground.move x y


mapAllPos : (Pos -> b) -> { a | width : Int, height : Int } -> List b
mapAllPos func mem =
    List.map func (allPos mem)


allPos : { a | width : Int, height : Int } -> List Pos
allPos { width, height } =
    List.range 0 (height - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (width - 1)
                    |> List.map (\x -> ( x, y ))
            )
