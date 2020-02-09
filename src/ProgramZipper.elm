module ProgramZipper exposing (ProgramZipper, down, init, left, right, setCD, setInst, up)

import Program exposing (CD, Inst, Program)


type alias ProgramZipper =
    { prog : Program
    , x : Int
    , y : Int
    }


init : Int -> Int -> ProgramZipper
init w h =
    ProgramZipper (Program.init w h) 0 0


down : ProgramZipper -> ProgramZipper
down =
    mapY inc


up : ProgramZipper -> ProgramZipper
up =
    mapY dec


left : ProgramZipper -> ProgramZipper
left =
    mapX dec


right : ProgramZipper -> ProgramZipper
right =
    mapX inc


setInst : Inst -> ProgramZipper -> ProgramZipper
setInst inst_ =
    mapPGXYA Program.setInst inst_


setCD : CD -> ProgramZipper -> ProgramZipper
setCD inst_ =
    mapPGXYA Program.setCD inst_


mapX : (Int -> Int) -> ProgramZipper -> ProgramZipper
mapX func b =
    { b | x = func b.x }


mapY : (Int -> Int) -> ProgramZipper -> ProgramZipper
mapY func b =
    { b | y = func b.y }


mapPGXYA : (Int -> Int -> a -> Program -> Program) -> a -> ProgramZipper -> ProgramZipper
mapPGXYA func a b =
    { b | prog = func b.x b.y a b.prog }


mapPG : (Program -> Program) -> ProgramZipper -> ProgramZipper
mapPG func b =
    { b | prog = b.prog }


inc =
    (+) 1


dec =
    (+) -1
