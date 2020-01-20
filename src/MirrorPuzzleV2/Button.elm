module MirrorPuzzleV2.Button exposing (Button, disable, findClicked, init, initWithBox, mapBox, view)

import List.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.Computer2 as Computer2
import MirrorPuzzleV2.MouseEvent as ME exposing (MouseEvent)
import Number2 exposing (Float2)
import Playground exposing (..)
import Playground.Extra exposing (..)


type alias Mouse =
    Computer2.Mouse


type Button a
    = Button (Model a)


type alias Model a =
    { data : a
    , label : String
    , box : Box
    , disabled : Bool
    }


initWithBox : a -> String -> Box -> Button a
initWithBox a txt box =
    Model a txt box False |> Button


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
    initWithBox a txt (Box.atOrigin width lineHeight)


disable : Button a -> Button a
disable (Button model) =
    Button { model | disabled = True }


mapBox : (Box -> Box) -> Button a -> Button a
mapBox func (Button model) =
    Button { model | box = func model.box }


view : Mouse -> Button a -> Shape
view mouse (Button model) =
    buttonShape (Box.contains mouse.pos model.box)
        (Box.dimensions model.box)
        model.label
        |> move2 (Box.center model.box)


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


isEnabled : Button a -> Bool
isEnabled (Button model) =
    not model.disabled


findClicked : MouseEvent -> List (Button a) -> Maybe a
findClicked ev buttons =
    ME.onClick
        (\pt ->
            buttons |> List.filter isEnabled |> findContaining pt
        )
        ev


findContaining : Float2 -> List (Button a) -> Maybe a
findContaining pt =
    List.Extra.find (contains pt)
        >> Maybe.map data


data : Button a -> a
data (Button model) =
    model.data


contains : Float2 -> Button a -> Bool
contains pt (Button model) =
    Box.contains pt model.box
