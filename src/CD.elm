module CD exposing (..)


type Arrow
    = Up
    | Down
    | Left
    | Right


arrowSymbol arrow =
    case arrow of
        Left ->
            "←"

        Right ->
            "→"

        Up ->
            "↑"

        Down ->
            "↓"


stepByArrow arrow x y =
    arrowToVec arrow |> stepByVec x y


stepByVec x y ( dx, dy ) =
    ( x + dx, y + dy )


fromArrow : Arrow -> CD
fromArrow =
    CD


toArrow : CD -> Maybe Arrow
toArrow cd =
    case cd of
        CD arrow ->
            Just arrow

        Cont ->
            Nothing


arrowToVec arrow =
    case arrow of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


type CD
    = CD Arrow
    | Cont


left =
    CD Left


right =
    CD Right


up =
    CD Up


down =
    CD Down
