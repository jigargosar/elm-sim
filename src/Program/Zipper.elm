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
import Prog exposing (Prog)


type alias Zipper =
    { prog : Prog
    , x : Int
    , y : Int
    , layerName : Prog.LayerName
    }


init : Int -> Int -> Zipper
init w h =
    Zipper (Prog.init w h) 0 0 Prog.red


switchToBlue : Zipper -> Zipper
switchToBlue =
    setLayerName Prog.blue


switchToRed : Zipper -> Zipper
switchToRed =
    setLayerName Prog.red


go : Int -> Int -> Zipper -> Zipper
go x y =
    mapX (always x) >> mapY (always y)


setLayerName : Prog.LayerName -> Zipper -> Zipper
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
    mapPGXYLA Prog.setInst inst_


setCD : CD -> Zipper -> Zipper
setCD inst_ =
    mapPGXYLA Prog.setCD inst_


set : Inst -> CD -> Zipper -> Zipper
set inst cd =
    setInst inst >> setCD cd


mapX : (Int -> Int) -> Zipper -> Zipper
mapX func b =
    { b | x = func b.x }


mapY : (Int -> Int) -> Zipper -> Zipper
mapY func b =
    { b | y = func b.y }


mapPGXYLA : (Prog.LayerName -> Int -> Int -> a -> Prog -> Prog) -> a -> Zipper -> Zipper
mapPGXYLA func val b =
    { b | prog = func b.layerName b.x b.y val b.prog }


mapPG : (Prog -> Prog) -> Zipper -> Zipper
mapPG func b =
    { b | prog = b.prog }


inc =
    (+) 1


dec =
    (+) -1
