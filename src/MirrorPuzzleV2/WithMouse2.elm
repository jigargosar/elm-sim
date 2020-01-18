module MirrorPuzzleV2.WithMouse2 exposing (game2)

import MirrorPuzzleV2.Mouse2 as Mouse2 exposing (Mouse2)
import Playground exposing (Computer, Shape, game)


type Model mem
    = Model Mouse2 mem


type alias View mem =
    Computer -> Mouse2 -> mem -> List Shape


type alias Update mem =
    Computer -> Mouse2 -> mem -> mem


view : View mem -> Computer -> Model mem -> List Shape
view func computer (Model mouse2 mem) =
    func computer mouse2 mem


update : Update mem -> Computer -> Model mem -> Model mem
update func computer (Model mouse2 mem) =
    let
        newMouse2 =
            Mouse2.update computer.mouse mouse2
    in
    func computer newMouse2 mem
        |> Model newMouse2


init : mem -> Model mem
init =
    Model Mouse2.initial


game2 v u i =
    game (view v) (update u) (init i)
