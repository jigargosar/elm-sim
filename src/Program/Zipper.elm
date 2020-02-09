module Program.Zipper exposing
    ( Zipper
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


type alias Zipper =
    { prog : Program
    , x : Int
    , y : Int
    , layerName : Program.LayerName
    }


init : Int -> Int -> Zipper
init w h =
    Zipper (Program.init w h) 0 0 Program.red


switchToBlue : Zipper -> Zipper
switchToBlue =
    setLayerName Program.blue


switchToRed : Zipper -> Zipper
switchToRed =
    setLayerName Program.red


go : Int -> Int -> Zipper -> Zipper
go x y =
    mapX (always x) >> mapY (always y)


setLayerName : Program.LayerName -> Zipper -> Zipper
setLayerName n z =
    { z | layerName = n }


goU : Zipper -> Zipper
goU =
    mapY dec


goD : Zipper -> Zipper
goD =
    mapY inc


goL : Zipper -> Zipper
goL =
    mapX dec


goR : Zipper -> Zipper
goR =
    mapX inc


setInst : Inst -> Zipper -> Zipper
setInst inst_ =
    mapPGXYLA Program.setInst inst_


setCD : CD -> Zipper -> Zipper
setCD inst_ =
    mapPGXYLA Program.setCD inst_


set : Inst -> CD -> Zipper -> Zipper
set inst cd =
    setInst inst >> setCD cd


mapX : (Int -> Int) -> Zipper -> Zipper
mapX func b =
    { b | x = func b.x }


mapY : (Int -> Int) -> Zipper -> Zipper
mapY func b =
    { b | y = func b.y }


mapPGXYLA : (Program.LayerName -> Int -> Int -> a -> Program -> Program) -> a -> Zipper -> Zipper
mapPGXYLA func val b =
    { b | prog = func b.layerName b.x b.y val b.prog }


mapPG : (Program -> Program) -> Zipper -> Zipper
mapPG func b =
    { b | prog = b.prog }


inc =
    (+) 1


dec =
    (+) -1
