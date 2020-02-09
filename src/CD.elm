module CD exposing (..)


type Arrow
    = Up
    | Down
    | Left
    | Right


type CD
    = CD Arrow
    | Cont


left =
    CD Left


right =
    CD Right


up =
    CD Up


down =
    CD Down
