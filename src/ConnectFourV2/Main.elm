module ConnectFourV2.Main exposing (main)

import Dict exposing (Dict)
import Playground exposing (..)


type Coin
    = Red
    | Blue


type alias Position =
    ( Int, Int )


type alias Mem =
    { coin : Coin
    , grid : Dict Position Coin
    , width : Int
    , height : Int
    }


initialMemory : Mem
initialMemory =
    { coin = Blue, grid = Dict.empty, width = 7, height = 6 }


updateMemory : Computer -> Mem -> Mem
updateMemory computer mem =
    mem


viewMemory : Computer -> Mem -> List Shape
viewMemory computer mem =
    [ viewBoard 50 mem.width mem.height (Dict.toList mem.grid) ]


viewBoard : Float -> Int -> Int -> List ( Position, Coin ) -> Shape
viewBoard cellSize w h list =
    let
        ( widthPx, heightPx ) =
            ( toFloat w * cellSize, toFloat h * cellSize )
    in
    group
        [ rectangle black widthPx heightPx
        ]


main =
    game viewMemory updateMemory initialMemory
