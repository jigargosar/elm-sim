module GameOfLifeCell exposing (..)


type Cell
    = On
    | Off


toggle : Cell -> Cell
toggle cell =
    case cell of
        On ->
            Off

        Off ->
            On
