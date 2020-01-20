module MirrorPuzzleV2.Button exposing (Button, init, mapBox, view)

import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.Computer2 as Computer2
import Playground exposing (..)
import Playground.Extra exposing (..)


type alias Mouse =
    Computer2.Mouse


type Button a
    = Button a String Box


init : a -> String -> Button a
init a txt =
    Button a txt (Box.atOrigin ((String.length txt + 10) * 16 |> toFloat) 16)


mapBox : (Box -> Box) -> Button a -> Button a
mapBox func (Button a txt box) =
    Button a txt (func box)


view : Mouse -> Button a -> Shape
view mouse (Button _ txt box) =
    buttonShape (Box.contains mouse.pos box)
        (Box.dimensions box)
        txt
        |> move2 (Box.center box)


buttonShape : Bool -> ( Number, Number ) -> String -> Shape
buttonShape hover ( w, h ) text =
    let
        thickness =
            3
    in
    [ rectangle black w h
    , rectangle
        (if hover then
            lightPurple

         else
            white
        )
        (w - thickness)
        (h - thickness)
    , words black text
    ]
        |> group
