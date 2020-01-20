module MirrorPuzzleV2.Button exposing (Button, findClicked, init, initWithBox, mapBox, view)

import List.Extra
import Maybe.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.Computer2 as Computer2
import MirrorPuzzleV2.MouseEvent as ME exposing (MouseEvent)
import Number2 exposing (Float2)
import Playground exposing (..)
import Playground.Extra exposing (..)


type alias Mouse =
    Computer2.Mouse


type Button a
    = Button a String Box


initWithBox : a -> String -> Box -> Button a
initWithBox a txt =
    Button a txt


init : a -> String -> Button a
init a txt =
    let
        fontSize =
            22

        lineHeight =
            fontSize * 1.3

        width =
            toFloat (String.length txt + 2) * (fontSize * 0.55)
    in
    Button a txt (Box.atOrigin width lineHeight)


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


findClicked : MouseEvent -> List (Button a) -> Maybe a
findClicked ev buttons =
    ME.onClick
        (\pt ->
            findContaining pt buttons
        )
        ev


findContaining : Float2 -> List (Button a) -> Maybe a
findContaining pt =
    List.Extra.find (contains pt)
        >> Maybe.map data


data : Button a -> a
data (Button a _ _) =
    a


contains : Float2 -> Button a -> Bool
contains pt (Button _ _ box) =
    Box.contains pt box
