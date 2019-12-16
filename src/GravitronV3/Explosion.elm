module GravitronV3.Explosion exposing (Explosion, explosionFrom, explosionToShape, updateExplosions)

import GravitronV3.Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Point exposing (Point)



-- Explosion


type alias Explosion =
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


updateExplosion : Explosion -> Maybe Explosion
updateExplosion explosion =
    if Counter.isDone explosion.timer then
        Nothing

    else
        Just { explosion | timer = Counter.step explosion.timer }


updateExplosions : List Explosion -> List Explosion
updateExplosions =
    List.filterMap updateExplosion


explosionToShape : Explosion -> Shape
explosionToShape { position, timer, shape } =
    let
        progress =
            Counter.progress timer
    in
    shape
        |> fade (1 - progress)
        |> scale (1 + (progress / 2))
