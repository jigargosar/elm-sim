module Program.Layer exposing (Layer, arrowAt, empty, instAt, setArrow, setCD, setInst)

import CD exposing (Arrow, CD)
import Grid exposing (Grid)
import Inst exposing (Inst)


type alias Layer =
    { instG : Grid Inst
    , moveG : Grid CD
    }


empty : Int -> Int -> Layer
empty w h =
    Layer (Grid.empty w h) (Grid.empty w h)


setInst : Int -> Int -> Inst -> Layer -> Layer
setInst x y inst l =
    { l | instG = Grid.set x y inst l.instG }


setCD : Int -> Int -> CD -> Layer -> Layer
setCD x y cd l =
    { l | moveG = Grid.set x y cd l.moveG }


setArrow : Int -> Int -> Arrow -> Layer -> Layer
setArrow x y arrow =
    setCD x y (CD.fromArrow arrow)


instAt : Int -> Int -> Layer -> Maybe Inst
instAt x y =
    .instG >> Grid.get x y


arrowAt : Int -> Int -> Layer -> Maybe Arrow
arrowAt x y =
    .moveG >> Grid.get x y >> Maybe.andThen CD.toArrow
