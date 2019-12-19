module GravitronV5.HP exposing (HP, fromMax, noneLeft, remaining, remove)


type HP
    = HP Int Int


fromMax : Int -> HP
fromMax mx =
    HP (max 0 mx) (max 0 mx)


remove : Int -> HP -> HP
remove hits (HP mx n) =
    HP mx (clamp 0 mx (n - hits))


remaining : HP -> Int
remaining (HP _ n) =
    n


noneLeft : HP -> Bool
noneLeft (HP _ n) =
    n <= 0
