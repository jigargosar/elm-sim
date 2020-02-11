module Prog exposing
    ( LayerName
    , Prog
    , arrowAt
    , blue
    , init
    , instAt
    , red
    , setArrow
    , setCD
    , setInst
    )

import CD exposing (Arrow, CD)
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


setArrow : LayerName -> Int -> Int -> Arrow -> Prog -> Prog
setArrow name x y cd =
    mapLayer name (Layer.setArrow x y cd)


instAt : LayerName -> Int -> Int -> Prog -> Maybe Inst
instAt name x y =
    getLayer name >> Layer.instAt x y


arrowAt : LayerName -> Int -> Int -> Prog -> Maybe Arrow
arrowAt name x y =
    getLayer name >> Layer.arrowAt x y


getLayer name =
    case name of
        Red ->
            .red

        Blue ->
            .blue


mapLayer : LayerName -> (Layer -> Layer) -> Prog -> Prog
mapLayer name func p =
    case name of
        Red ->
            { p | red = func p.red }

        Blue ->
            { p | blue = func p.blue }
