module Program.Builder exposing (Builder, Config, LayerConfig, Step, build, exe, exe2, init, startAt, step, stepIn)

import CD exposing (Arrow)
import Inst exposing (Inst)
import Prog exposing (Prog)


type alias Builder =
    { x : Int
    , y : Int
    , arrow : Arrow
    , layer : Prog.LayerName
    , prog : Prog
    }


init : Int -> Int -> Builder
init w h =
    Builder 0 0 CD.Left Prog.red (Prog.init w h)


type alias Config =
    { width : Int
    , height : Int
    , blue : LayerConfig
    , red : LayerConfig
    }


type alias LayerConfig =
    { x : Int
    , y : Int
    , arrow : Arrow
    , steps : List Step
    }


type alias Step =
    Builder -> Builder


build : Config -> Prog
build config =
    init config.width config.height
        |> switchRed
        |> startAt config.red.x config.red.y config.red.arrow
        |> applyAll config.red.steps
        |> switchBlue
        |> startAt config.blue.x config.blue.y config.blue.arrow
        |> applyAll config.blue.steps
        |> .prog


applyAll : List Step -> Builder -> Builder
applyAll steps b =
    List.foldl (<|) b steps


switchRed : Builder -> Builder
switchRed b =
    { b | layer = Prog.red }


switchBlue b =
    { b | layer = Prog.blue }


startAt : Int -> Int -> Arrow -> Builder -> Builder
startAt x y arrow =
    setXY x y >> setI Inst.start >> stepIn arrow


step : Builder -> Builder
step b =
    let
        { x, y, arrow, prog } =
            b

        ( nx, ny ) =
            CD.stepByArrow arrow x y
    in
    { b | x = nx, y = ny }


stepIn : Arrow -> Builder -> Builder
stepIn arrow b =
    let
        { x, y, layer, prog } =
            b
    in
    { b | x = x, y = y, arrow = arrow, prog = Prog.setArrow layer x y arrow prog }
        |> step


exe : Inst -> Builder -> Builder
exe i =
    setI i >> step


exe2 : Inst -> Arrow -> Builder -> Builder
exe2 i a =
    setI i >> stepIn a


setI : Inst -> Builder -> Builder
setI inst b =
    let
        { x, y, layer, prog } =
            b
    in
    { b | prog = Prog.setInst layer x y inst prog }


setXY : Int -> Int -> Builder -> Builder
setXY x y b =
    { b | x = x, y = y }
