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


type alias Pos =
    ( Int, Int )


type alias Config =
    { cellSize : Float
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
    , toScreen = toScreen
    , toGrid = toGrid
    , width = cellSize * toFloat mem.width
    , height = cellSize * toFloat mem.height
    }


view : Computer -> Mem -> List Shape
view { screen } mem =
    let
        cfg =
            toConfig screen mem
    in
    []


main =
    game view update init
