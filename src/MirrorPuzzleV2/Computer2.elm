module MirrorPuzzleV2.Computer2 exposing (..)

import MirrorPuzzleV2.Mouse2 as Mouse2 exposing (Mouse2)
import MirrorPuzzleV2.MouseEvent exposing (MouseEvent)
import Number2 exposing (Float2)
import Playground exposing (..)


type alias Mouse =
    { x : Number
    , y : Number
    , pos : Float2
    , down : Bool
    , click : Bool
    , event : MouseEvent
    }


type alias Computer2 =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


init : Computer -> Mouse2 -> Computer2
init { mouse, keyboard, screen, time } mouse2 =
    { mouse = toMouse mouse mouse2, keyboard = keyboard, screen = screen, time = time }


toMouse : Playground.Mouse -> Mouse2 -> Mouse
toMouse { x, y, down, click } mouse2 =
    { x = x, y = y, pos = ( x, y ), down = down, click = click, event = Mouse2.toEvent mouse2 }
