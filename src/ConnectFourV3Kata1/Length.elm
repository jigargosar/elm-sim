module ConnectFourV3Kata1.Length exposing (Length, init, member)


type Length
    = Len Int


init : Int -> Length
init len =
    Len (abs len)


member : Int -> Length -> Bool
member idx (Len len) =
    (idx < 0 || idx >= len)
        |> not
