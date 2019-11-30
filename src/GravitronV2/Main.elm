module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)


type alias Memory =
    { x : Float
    }


initialMemory =
    { x = -100 }


main : Game Memory
main =
    game initialMemory [ circle -110 0 10 red ]
