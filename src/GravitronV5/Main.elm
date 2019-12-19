module GravitronV5.Main exposing (main)

import Playground exposing (..)


type alias Mem =
    {}


initialMemory : Mem
initialMemory =
    {}


updateMemory : Computer -> Mem -> Mem
updateMemory { time, screen, mouse } mem =
    mem


viewMemory : Computer -> Mem -> List Shape
viewMemory _ _ =
    []


main =
    Playground.game viewMemory updateMemory initialMemory
