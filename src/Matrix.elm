module Matrix exposing (..)

import Array exposing (Array)


type alias Matrix a =
    { rc : Int
    , cc : Int
    , vec : Array a
    }


repeat : Int -> Int -> a -> Matrix a
repeat rc cc a =
    Matrix rc cc (Array.repeat (rc * cc) a)
