module ProgramGrid exposing (CD, Inst, LayerLabel, ProgramGrid, init, setInst, setMove)

import Grid exposing (Grid)


type GrabDrop
    = Grab
    | Drop
    | GrabOrDrop


type InputChannel
    = Alpha
    | Beta


type OutputChannel
    = Psi
    | Omega


type Inst
    = Start
    | GrabDrop GrabDrop
    | In InputChannel
    | Out OutputChannel


type Arrow
    = Up
    | Down
    | Left
    | Right


type CD
    = CD Arrow
    | Cont


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


type alias ProgramGrid =
    { red : Layer
    , blue : Layer
    }


init : Int -> Int -> ProgramGrid
init w h =
    ProgramGrid (emptyLayer w h) (emptyLayer w h)


setInst : Int -> Int -> Inst -> ProgramGrid -> ProgramGrid
setInst =
    Debug.todo "impl"


setMove : Int -> Int -> CD -> ProgramGrid -> ProgramGrid
setMove =
    Debug.todo "impl"
