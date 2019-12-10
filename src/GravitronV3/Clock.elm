module GravitronV3.Clock exposing (Clock, initial)

import Time exposing (Posix)


type Clock
    = Clock Posix


initial : Clock
initial =
    Clock (Time.millisToPosix 0)
