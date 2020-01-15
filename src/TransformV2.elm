module TransformV2 exposing
    ( Transform
    , identity
    , inverse
    , inverseRound
    , scale
    , scale2
    , transform
    , transformI
    , translate
    )

import NumberTuple as NT
import PointFree exposing (flip)


type Transform
    = Transform (List Type)


type Type
    = Scale NT.Float
    | Translate NT.Float


compose : Type -> Transform -> Transform
compose type_ (Transform list0) =
    let
        list =
            case ( type_, list0 ) of
                ( _, [] ) ->
                    [ type_ ]

                ( Scale new, (Scale old) :: rest ) ->
                    Scale (NT.mul old new) :: rest

                ( Translate new, (Translate old) :: rest ) ->
                    Translate (NT.add old new) :: rest

                _ ->
                    type_ :: list0
    in
    Transform list


identity : Transform
identity =
    Transform []


scale2 : NT.Float -> Transform -> Transform
scale2 =
    Scale >> compose


scale : Float -> Transform -> Transform
scale s =
    scale2 ( s, s )


translate : NT.Float -> Transform -> Transform
translate =
    Translate >> compose


transform : Transform -> NT.Float -> NT.Float
transform (Transform list) point =
    List.foldr
        (\t ->
            case t of
                Scale s ->
                    NT.mul s

                Translate dt ->
                    NT.add dt
        )
        point
        list


inverse : Transform -> NT.Float -> NT.Float
inverse (Transform list) point =
    List.foldl
        (\t ->
            case t of
                Scale s ->
                    flip NT.div s

                Translate dt ->
                    flip NT.sub dt
        )
        point
        list


inverseRound : Transform -> NT.Float -> NT.Int
inverseRound t =
    inverse t >> NT.round


transformI : Transform -> NT.Int -> NT.Float
transformI t =
    NT.toFloat >> transform t
