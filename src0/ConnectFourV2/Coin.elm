module ConnectFourV2.Coin exposing (Coin(..), flip)


type Coin
    = Blue
    | Red


flip coin =
    case coin of
        Blue ->
            Red

        Red ->
            Blue
