module Transform exposing (Transform, inverse, scale, scale2, transform, translate)

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


transform : NT.Float -> List Transform -> NT.Float
transform =
    List.foldl
        (\t ->
            case t of
                Scale2 ( 1, 1 ) ->
                    identity

                Scale2 s ->
                    NT.mul s

                Translate dt ->
                    NT.add dt
        )


inverse : NT.Float -> List Transform -> NT.Float
inverse =
    List.foldr
        (\t ->
            case t of
                Scale2 ( 1, 1 ) ->
                    identity

                Translate ( 0, 0 ) ->
                    identity

                Scale2 s ->
                    flip NT.div s

                Translate dt ->
                    flip NT.sub dt
        )
