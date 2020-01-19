module MirrorPuzzleV2.Game2 exposing (game2)

import MirrorPuzzleV2.Computer2 as Computer2 exposing (Computer2)
import MirrorPuzzleV2.Mouse2 as Mouse2 exposing (Mouse2)
import Playground exposing (..)


type Model mem
    = Model Mouse2 mem


init : mem -> Model mem
init =
    Model Mouse2.initial


update : (Computer2 -> mem -> mem) -> Computer -> Model mem -> Model mem
update func computer =
    mapMouse2 (Mouse2.update computer.mouse)
        >> updateMem computer func


mapMouse2 : (Mouse2 -> Mouse2) -> Model mem -> Model mem
mapMouse2 func (Model mouse2 mem) =
    Model (func mouse2) mem


updateMem : Computer -> (Computer2 -> mem -> mem) -> Model mem -> Model mem
updateMem computer func (Model mouse2 mem) =
    func (Computer2.init computer mouse2) mem |> Model mouse2


view : (Computer2 -> mem -> List Shape) -> Computer -> Model mem -> List Shape
view func computer (Model mouse2 mem) =
    func (Computer2.init computer mouse2) mem


game2 v u i =
    game (view v) (update u) (init i)
