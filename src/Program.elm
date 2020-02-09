module Program exposing (CD, Inst, LayerName, Program, blue, init, red, setCD, setInst)

import CD
import Grid exposing (Grid)
import Inst


type alias Inst =
    Inst.Inst


type alias CD =
    CD.CD


type alias Layer =
    { instG : Grid Inst
    , moveG : Grid CD
    }


emptyLayer : Int -> Int -> Layer
emptyLayer w h =
    Layer (Grid.empty w h) (Grid.empty w h)


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
    Program (emptyLayer w h) (emptyLayer w h)


setInst : LayerName -> Int -> Int -> Inst -> Program -> Program
setInst name x y inst =
    mapLayer name (setInstInLayer x y inst)


setCD : LayerName -> Int -> Int -> CD -> Program -> Program
setCD name x y cd =
    mapLayer name (setCDInLayer x y cd)


setInstInLayer : Int -> Int -> Inst -> Layer -> Layer
setInstInLayer x y inst l =
    { l | instG = Grid.set x y inst l.instG }


setCDInLayer : Int -> Int -> CD -> Layer -> Layer
setCDInLayer x y cd l =
    { l | moveG = Grid.set x y cd l.moveG }


mapLayer : LayerName -> (Layer -> Layer) -> Program -> Program
mapLayer name func p =
    case name of
        Red ->
            { p | red = func p.red }

        Blue ->
            { p | blue = func p.blue }
