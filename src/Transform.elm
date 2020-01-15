module Transform exposing (Transform, inverse, inverseRound, scale, scale2, transform, transformI, transformOrigin, translate)

import NumberTuple as NT
import PointFree exposing (flip)


type Transform
    = Scale2 NT.Float
    | Translate NT.Float


scale2 : NT.Float -> Transform
scale2 =
    Scale2


scale : Float -> Transform
scale s =
    scale2 ( s, s )


translate : NT.Float -> Transform
translate =
    Translate


transformOrigin : List Transform -> NT.Float
transformOrigin t =
    transform t ( 0, 0 )


transformI : List Transform -> NT.Int -> NT.Float
transformI t =
    NT.toFloat >> transform t


transform : List Transform -> NT.Float -> NT.Float
transform =
    List.foldl
        (\t ->
            case t of
                Scale2 s ->
                    NT.mul s

                Translate dt ->
                    NT.add dt
        )
        |> flip


inverseRound : List Transform -> NT.Float -> NT.Int
inverseRound t =
    inverse t >> NT.round


inverse : List Transform -> NT.Float -> NT.Float
inverse =
    List.foldr
        (\t ->
            case t of
                Scale2 s ->
                    flip NT.div s

                Translate dt ->
                    flip NT.sub dt
        )
        |> flip
