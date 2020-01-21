module Length exposing (member)


member : Int -> Int -> Bool
member index length =
    index >= 0 && index <= length
