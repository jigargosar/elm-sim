module ConnectFourV3.Main exposing (main)

import Dict exposing (Dict)
import Playground exposing (..)


type alias Position =
    ( Int, Int )


type Coin
    = Red
    | Blue


columns =
    7


rows =
    6


type alias Mem =
    { board : Dict Position Coin
    }


initialMemory : Mem
initialMemory =
    { board = Dict.empty }



-- makeMove column mem =


updateMemory : Computer -> Mem -> Mem
updateMemory _ mem =
    mem


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { board } =
    let
        cellRadius =
            25

        cellSize =
            cellRadius * 2

        width =
            columns * cellSize

        height =
            rows * cellSize
    in
    [ group
        [ rectangle black width height
        ]
    ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
