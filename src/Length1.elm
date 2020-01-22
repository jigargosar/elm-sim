module Length1 exposing (member)


member : Int -> Int -> Bool
member index length =
    index >= 0 && index <= length
