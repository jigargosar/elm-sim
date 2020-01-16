module TransformV2 exposing
    ( Transform
    , identity
    , inverse
    , inverseRound
    , move
    , moveDown
    , moveRight
    , moveX
    , moveY
    , scale
    , scale2
    , transform
    , transformI
    , translate
    )

import Number2 as NT
import PointFree exposing (flip)


type Transform
    = Transform (List Type)


type Type
    = Scale NT.Float2
    | Translate NT.Float2


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


scale2 : NT.Float2 -> Transform -> Transform
scale2 =
    Scale >> compose


scale : Float -> Transform -> Transform
scale s =
    scale2 ( s, s )


translate : NT.Float2 -> Transform -> Transform
translate =
    Translate >> compose


transform : Transform -> NT.Float2 -> NT.Float2
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


inverse : Transform -> NT.Float2 -> NT.Float2
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


inverseRound : Transform -> NT.Float2 -> NT.Int2
inverseRound t =
    inverse t >> NT.round


transformI : Transform -> NT.Int2 -> NT.Float2
transformI t =
    NT.toFloat >> transform t


move : NT.Float2 -> Transform -> Transform
move =
    translate


moveX : Float -> Transform -> Transform
moveX dx =
    move ( dx, 0 )


moveY : Float -> Transform -> Transform
moveY dy =
    move ( 0, dy )


moveDown : Float -> Transform -> Transform
moveDown dy =
    moveY -dy


moveRight : Float -> Transform -> Transform
moveRight =
    moveX
