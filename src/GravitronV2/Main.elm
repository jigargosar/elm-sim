module GravitronV2.Main exposing (main)

import GravitronV2.Draw exposing (..)


type alias Memory =
    { x : Float
    }


initialMemory =
    { x = -100 }


view : Computer -> Memory -> List Shape
view computer memory =
    [ circle memory.x 0 10 red ]


main : Game Memory
main =
    game initialMemory view
