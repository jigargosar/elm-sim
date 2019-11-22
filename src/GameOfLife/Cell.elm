module GameOfLife.Cell exposing (Cell, nextState, off, on, toggle)


type Cell
    = On
    | Off


on : Cell
on =
    On


off : Cell
off =
    Off


toggle : Cell -> Cell
toggle cell =
    case cell of
        On ->
            Off

        Off ->
            On


nextState : Int -> Int -> (Int -> Int -> Maybe Cell) -> Cell -> Cell
nextState rowIdx colIdx cellAt =
    aliveNeighbourCountOfCellAtRC rowIdx colIdx cellAt
        |> nextStateWithAliveNeighbourCount


neighbours : List ( Int, Int )
neighbours =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


aliveNeighbourCountOfCellAtRC : Int -> Int -> (Int -> Int -> Maybe Cell) -> Int
aliveNeighbourCountOfCellAtRC rowNum colNum cellAt =
    neighbours
        |> List.foldl
            (\( dr, dc ) ct ->
                case cellAt (rowNum + dr) (colNum + dc) of
                    Just On ->
                        ct + 1

                    _ ->
                        ct
            )
            0


{-| <https://www.conwaylife.com/wiki/Conway's_Game_of_Life>
-}
nextStateWithAliveNeighbourCount : Int -> Cell -> Cell
nextStateWithAliveNeighbourCount aliveNeighbourCount cell =
    case cell of
        On ->
            {- Any live cell with fewer than two live neighbours dies
                (referred to as underpopulation or exposure[1]).
               Any live cell with more than three live neighbours dies
                (referred to as overpopulation or overcrowding).
            -}
            if aliveNeighbourCount < 2 || aliveNeighbourCount > 3 then
                Off

            else
                {- Any live cell with two or three live neighbours lives,
                   unchanged, to the next generation.
                -}
                On

        Off ->
            if aliveNeighbourCount == 3 then
                On

            else
                Off
