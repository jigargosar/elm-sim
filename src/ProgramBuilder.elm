module ProgramBuilder exposing (ProgramBuilder, down, init)

import ProgramGrid exposing (Inst, ProgramGrid)


type alias ProgramBuilder =
    { pg : ProgramGrid
    , x : Int
    , y : Int
    }


init : Int -> Int -> ProgramBuilder
init w h =
    ProgramBuilder (ProgramGrid.init w h) 0 0


mapX : (Int -> Int) -> ProgramBuilder -> ProgramBuilder
mapX func b =
    { b | x = func b.x }


mapY : (Int -> Int) -> ProgramBuilder -> ProgramBuilder
mapY func b =
    { b | y = func b.y }


down : ProgramBuilder -> ProgramBuilder
down =
    mapY inc


up : ProgramBuilder -> ProgramBuilder
up =
    mapY dec


left : ProgramBuilder -> ProgramBuilder
left =
    mapX dec


right : ProgramBuilder -> ProgramBuilder
right =
    mapX inc


inst : Inst -> ProgramBuilder -> ProgramBuilder
inst inst_ =
    mapPGXYA ProgramGrid.setInst inst_


mapPGXYA : (Int -> Int -> a -> ProgramGrid -> ProgramGrid) -> a -> ProgramBuilder -> ProgramBuilder
mapPGXYA func a b =
    { b | pg = func b.x b.y a b.pg }


mapPG : (ProgramGrid -> ProgramGrid) -> ProgramBuilder -> ProgramBuilder
mapPG func b =
    { b | pg = b.pg }


inc =
    (+) 1


dec =
    (+) -1
