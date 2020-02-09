module Inst exposing (..)


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


start =
    Start


grab =
    GrabDrop Grab
