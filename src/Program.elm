module Program exposing
    ( LayerName
    , Program
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


type alias Program =
    { red : Layer
    , blue : Layer
    }


init : Int -> Int -> Program
init w h =
    Program (Layer.empty w h) (Layer.empty w h)


setInst : LayerName -> Int -> Int -> Inst -> Program -> Program
setInst name x y inst =
    mapLayer name (Layer.setInst x y inst)


setCD : LayerName -> Int -> Int -> CD -> Program -> Program
setCD name x y cd =
    mapLayer name (Layer.setCD x y cd)


mapLayer : LayerName -> (Layer -> Layer) -> Program -> Program
mapLayer name func p =
    case name of
        Red ->
            { p | red = func p.red }

        Blue ->
            { p | blue = func p.blue }
