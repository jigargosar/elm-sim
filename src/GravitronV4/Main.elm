module GravitronV4.Main exposing (..)

import Playground exposing (Computer, Shape)


type Point
    = Point Float Float


type alias Player =
    { position : Point }


type alias Memory =
    {}


viewMemory : Computer -> Memory -> List Shape
viewMemory _ _ =
    []


updateMemory _ =
    identity


initialMemory : Memory
initialMemory =
    {}


main =
    Playground.game viewMemory updateMemory initialMemory
