module Prog exposing
    ( LayerName
    , Prog
    , blue
    , init
    , red
    , setCD
    , setInst
    )

import CD exposing (CD)
import Inst exposing (Inst)
import Program.Layer as Layer exposing (Layer)


type LayerName
    = Red
    | Blue


blue : LayerName
blue =
    Blue


red : LayerName
red =
    Red


type alias Prog =
    { red : Layer
    , blue : Layer
    }


init : Int -> Int -> Prog
init w h =
    Prog (Layer.empty w h) (Layer.empty w h)


setInst : LayerName -> Int -> Int -> Inst -> Prog -> Prog
setInst name x y inst =
    mapLayer name (Layer.setInst x y inst)


setCD : LayerName -> Int -> Int -> CD -> Prog -> Prog
setCD name x y cd =
    mapLayer name (Layer.setCD x y cd)


mapLayer : LayerName -> (Layer -> Layer) -> Prog -> Prog
mapLayer name func p =
    case name of
        Red ->
            { p | red = func p.red }

        Blue ->
            { p | blue = func p.blue }
