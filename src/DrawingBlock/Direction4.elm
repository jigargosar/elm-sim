module DrawingBlock.Direction4 exposing (Label(..), opposite, step)

import Number2 as NT exposing (Int2)


type Label
    = Up
    | Down
    | Left
    | Right


step : ( number, number ) -> Label -> ( number, number )
step pos =
    toVec >> NT.add pos


opposite : Label -> Label
opposite d =
    case d of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


toVec : Label -> ( number, number )
toVec d =
    case d of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )
