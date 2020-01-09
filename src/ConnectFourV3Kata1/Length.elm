module ConnectFourV3Kata1.Length exposing (Length, fromInt, member, toInt)


type Length
    = Len Int


fromInt : Int -> Length
fromInt len =
    Len (abs len)


member : Int -> Length -> Bool
member idx (Len len) =
    (idx < 0 || idx >= len)
        |> not


toInt : Length -> Int
toInt (Len len) =
    len
