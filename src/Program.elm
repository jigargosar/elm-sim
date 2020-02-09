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
setInst =
    Debug.todo "impl"


setCD : LayerName -> Int -> Int -> CD -> Program -> Program
setCD =
    Debug.todo "impl"
