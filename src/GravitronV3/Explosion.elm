module GravitronV3.Explosion exposing (Explosion, explosionFrom, updateExplosions, viewAll)

import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Point as Point exposing (Point)



-- Explosion


type Explosion
    = Explosion Record


type alias Record =
    { position : Point
    , shape : Shape
    , timer : Counter
    }


explosionFrom :
    ({ a | position : Point } -> Shape)
    -> { a | position : Point }
    -> Explosion
explosionFrom func entity =
    { position = entity.position
    , shape = func entity
    , timer = Counter.init 60
    }
        |> Explosion


updateExplosion : Explosion -> Maybe Explosion
updateExplosion (Explosion rec) =
    if Counter.isDone rec.timer then
        Nothing

    else
        Just (Explosion { rec | timer = Counter.step rec.timer })


updateExplosions : List Explosion -> List Explosion
updateExplosions =
    List.filterMap updateExplosion


toShape : Explosion -> Shape
toShape (Explosion { position, timer, shape }) =
    let
        progress =
            Counter.progress timer
    in
    shape
        |> fade (1 - progress)
        |> scale (1 + (progress / 2))


view : Explosion -> Shape
view ((Explosion { position }) as explosion) =
    toShape explosion
        |> Canvas.move (Point.toTuple position)


viewAll : List Explosion -> Shape
viewAll =
    List.map view >> group
