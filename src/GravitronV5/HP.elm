module GravitronV5.HP exposing (HP, noneLeft, remaining, remove, withMax)


type HP
    = HP Int Int


withMax : Int -> HP
withMax mx =
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
