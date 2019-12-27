module ConnectFour.Board exposing (Board, empty)


type Coin
    = RedCoin
    | BlueCoin


type Board
    = Board


empty : Int -> Int -> Board
empty =
    Debug.todo "impl"


type Error
    = RowFull
    | InvalidRow


addCoinInRow : Int -> Coin -> Result Error Board
addCoinInRow =
    Debug.todo "impl"
