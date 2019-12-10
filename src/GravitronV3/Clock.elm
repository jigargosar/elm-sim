module GravitronV3.Clock exposing (Clock, initial, onAnimationFrame)

import Time exposing (Posix)


type Clock
    = Clock Posix


initial : Clock
initial =
    Clock (Time.millisToPosix 0)


onAnimationFrame : Posix -> Clock -> Clock
onAnimationFrame posix (Clock _) =
    Clock posix
