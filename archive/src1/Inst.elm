module Inst exposing (..)


type InputChannel
    = Alpha
    | Beta


type OutputChannel
    = Psi
    | Omega


type Inst
    = Start
    | Grab
    | Drop
    | In InputChannel
    | Out OutputChannel


start =
    Start


grab =
    Grab


alphaInput =
    In Alpha


psiOutput =
    Out Psi


drop =
    Drop
