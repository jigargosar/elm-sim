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
    []


main =
    game viewMemory updateMemory initialMemory
