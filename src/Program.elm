module Program exposing (CD, Inst, LayerLabel, Program, init, setCD, setInst)

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


type LayerLabel
    = Red
    | Blue


type alias Program =
    { red : Layer
    , blue : Layer
    }


init : Int -> Int -> Program
init w h =
    Program (emptyLayer w h) (emptyLayer w h)


setInst : Int -> Int -> Inst -> Program -> Program
setInst =
    Debug.todo "impl"


setCD : Int -> Int -> CD -> Program -> Program
setCD =
    Debug.todo "impl"
