module GravitronV4.Main exposing (main)

import Playground exposing (..)


type alias Player =
    { x : Number
    , y : Number
    }


type alias Memory =
    { player : Player }


viewMemory : Computer -> Memory -> List Shape
viewMemory _ { player } =
    [ viewPlayer player ]


viewPlayer : Player -> Shape
viewPlayer { x, y } =
    circle green 20
        |> move x y


updateMemory _ =
    identity


initialMemory : Memory
initialMemory =
    { player = Player 0 0 }


main =
    Playground.game viewMemory updateMemory initialMemory
