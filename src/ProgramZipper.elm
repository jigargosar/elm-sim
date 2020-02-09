module ProgramZipper exposing
    ( ProgramZipper
    , go
    , goD
    , goL
    , goR
    , goU
    , init
    , set
    , setCD
    , setInst
    , switchToBlue
    , switchToRed
    )

import CD exposing (CD)
import Inst exposing (Inst)
import Program exposing (Program)


type alias ProgramZipper =
    { prog : Program
    , x : Int
    , y : Int
    , layerName : Program.LayerName
    }


init : Int -> Int -> ProgramZipper
init w h =
    ProgramZipper (Program.init w h) 0 0 Program.red


switchToBlue : ProgramZipper -> ProgramZipper
switchToBlue =
    setLayerName Program.blue


switchToRed : ProgramZipper -> ProgramZipper
switchToRed =
    setLayerName Program.red


go : Int -> Int -> ProgramZipper -> ProgramZipper
go x y =
    mapX (always x) >> mapY (always y)


setLayerName : Program.LayerName -> ProgramZipper -> ProgramZipper
setLayerName n z =
    { z | layerName = n }


goU : ProgramZipper -> ProgramZipper
goU =
    mapY dec


goD : ProgramZipper -> ProgramZipper
goD =
    mapY inc


goL : ProgramZipper -> ProgramZipper
goL =
    mapX dec


goR : ProgramZipper -> ProgramZipper
goR =
    mapX inc


setInst : Inst -> ProgramZipper -> ProgramZipper
setInst inst_ =
    mapPGXYLA Program.setInst inst_


setCD : CD -> ProgramZipper -> ProgramZipper
setCD inst_ =
    mapPGXYLA Program.setCD inst_


set : Inst -> CD -> ProgramZipper -> ProgramZipper
set inst cd =
    setInst inst >> setCD cd


mapX : (Int -> Int) -> ProgramZipper -> ProgramZipper
mapX func b =
    { b | x = func b.x }


mapY : (Int -> Int) -> ProgramZipper -> ProgramZipper
mapY func b =
    { b | y = func b.y }


mapPGXYLA : (Program.LayerName -> Int -> Int -> a -> Program -> Program) -> a -> ProgramZipper -> ProgramZipper
mapPGXYLA func val b =
    { b | prog = func b.layerName b.x b.y val b.prog }


mapPG : (Program -> Program) -> ProgramZipper -> ProgramZipper
mapPG func b =
    { b | prog = b.prog }


inc =
    (+) 1


dec =
    (+) -1
