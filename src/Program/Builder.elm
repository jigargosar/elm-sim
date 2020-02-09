module Program.Builder exposing (Builder, build, exe, init, startAt, step, stepIn)

import CD
import Inst exposing (Inst)
import Prog exposing (Prog)


type alias Builder =
    { x : Int, y : Int, arrow : CD.Arrow, prog : Prog }


init : Int -> Int -> Builder
init w h =
    Builder 0 0 CD.Left (Prog.init w h)


startAt : Int -> Int -> Builder -> Builder
startAt x y =
    setXY x y >> setI Inst.start


step : Builder -> Builder
step b =
    let
        { x, y, arrow, prog } =
            b

        ( nx, ny ) =
            CD.stepByArrow arrow x y
    in
    { b | x = nx, y = ny }


stepIn : CD.Arrow -> Builder -> Builder
stepIn arrow b =
    let
        { x, y, prog } =
            b
    in
    { b | x = x, y = y, arrow = arrow, prog = Prog.setCD Prog.blue x y (CD.fromArrow arrow) prog }
        |> step


exe : Inst -> Builder -> Builder
exe =
    setI


build : Builder -> Prog
build =
    .prog


setI : Inst -> Builder -> Builder
setI inst b =
    let
        { x, y, prog } =
            b
    in
    { b | prog = Prog.setInst Prog.blue x y inst prog }


setXY : Int -> Int -> Builder -> Builder
setXY x y b =
    { b | x = x, y = y }