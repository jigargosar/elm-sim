module GravitronV3.Clock exposing (Clock, initial, onAnimationFrame, timer)

import GravitronV3.Timer as Timer
import Time exposing (Posix)


type Clock
    = Clock Posix


initial : Clock
initial =
    Clock (Time.millisToPosix 0)


onAnimationFrame : Posix -> Clock -> Clock
onAnimationFrame posix (Clock _) =
    Clock posix


timer : Float -> Clock -> Timer.Timer
timer duration (Clock posix) =
    Timer.start (Time.posixToMillis posix |> toFloat) duration
