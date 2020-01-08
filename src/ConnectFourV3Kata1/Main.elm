module ConnectFourV3Kata1.Main exposing (main)

import Dict exposing (Dict)
import Playground exposing (..)


type alias Pos =
    ( Int, Int )


type Coin
    = Blue
    | Red


type alias Mem =
    { width : Int
    , height : Int
    , grid : Dict Pos Coin
    }


init : Mem
init =
    Mem 7 6 Dict.empty


update : Computer -> Mem -> Mem
update computer mem =
    mem


view : Computer -> Mem -> List Shape
view computer mem =
    []


main =
    game view update init
