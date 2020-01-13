module Playground.Extra exposing (inDegrees, line, noShape)

import Playground exposing (..)


noShape : Shape
noShape =
    group []


line : Color -> Number -> ( Number, Number ) -> ( Number, Number ) -> Shape
line color thickness ( x1, y1 ) ( x2, y2 ) =
    let
        ( dx, dy ) =
            ( x2 - x1, y2 - y1 )

        ( len, degrees ) =
            -- ( sqrt (dx ^ 2 + dy ^ 2), atan2 dy dx )
            toPolar ( dx, dy )
                |> Tuple.mapSecond inDegrees
    in
    rectangle color len thickness
        |> rotate degrees
        |> move (x1 + dx / 2) (y1 + dy / 2)


inDegrees : Float -> Float
inDegrees angle =
    angle / degrees 1
