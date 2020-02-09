module Program.Layer exposing (Layer, empty, setCD, setInst)

import CD exposing (CD)
import Grid exposing (Grid)
import Inst exposing (Inst)


type alias Layer =
    { instG : Grid Inst
    , moveG : Grid CD
    }


empty : Int -> Int -> Layer
empty w h =
    Layer (Grid.empty w h) (Grid.empty w h)


setInst : Int -> Int -> Inst -> Layer -> Layer
setInst x y inst l =
    { l | instG = Grid.set x y inst l.instG }


setCD : Int -> Int -> CD -> Layer -> Layer
setCD x y cd l =
    { l | moveG = Grid.set x y cd l.moveG }
