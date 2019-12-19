module GravitronV5.HP exposing (HP, decHPBy, initHP, noHPLeft, remainingHP)


type HP
    = HP Int Int


initHP : Int -> HP
initHP mx =
    HP (max 0 mx) (max 0 mx)


decHPBy : Int -> HP -> HP
decHPBy hits (HP mx n) =
    HP mx (clamp 0 mx (n - hits))


remainingHP : HP -> Int
remainingHP (HP _ n) =
    n


noHPLeft : HP -> Bool
noHPLeft (HP _ n) =
    n <= 0
