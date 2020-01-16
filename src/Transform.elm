module Transform exposing
    ( Transform
    , inverse
    , inverseRound
    , scale
    , scale2
    , transform
    , transformI
    , transformOrigin
    , translate
    )

import Number2 as NT
import PointFree exposing (flip)


type Transform
    = Scale NT.Float2
    | Translate NT.Float2


scale2 : NT.Float2 -> Transform
scale2 =
    Scale


scale : Float -> Transform
scale s =
    scale2 ( s, s )


translate : NT.Float2 -> Transform
translate =
    Translate


transformOrigin : List Transform -> NT.Float2
transformOrigin t =
    transform t ( 0, 0 )


transformI : List Transform -> NT.Int2 -> NT.Float2
transformI t =
    NT.toFloat >> transform t


transform : List Transform -> NT.Float2 -> NT.Float2
transform =
    List.foldl
        (\t ->
            case t of
                Scale s ->
                    NT.mul s

                Translate dt ->
                    NT.add dt
        )
        |> flip


inverseRound : List Transform -> NT.Float2 -> NT.Int2
inverseRound t =
    inverse t >> NT.round


inverse : List Transform -> NT.Float2 -> NT.Float2
inverse =
    List.foldr
        (\t ->
            case t of
                Scale s ->
                    flip NT.div s

                Translate dt ->
                    flip NT.sub dt
        )
        |> flip
