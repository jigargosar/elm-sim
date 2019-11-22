module MatrixOfLife exposing
    ( Cell(..)
    , Grid
    , indexedMapToList
    , initEmpty
    , nextState
    , randomize
    , toggleCellAtRC
    )

import Matrix exposing (Matrix)
import Random exposing (Generator)


type Cell
    = On
    | Off


type alias Grid =
    Matrix Cell


toggleCellAtRC : Int -> Int -> Grid -> Grid
toggleCellAtRC rowNum colNum =
    Matrix.mapAt rowNum colNum toggleCell


toggleCell cell =
    case cell of
        On ->
            Off

        Off ->
            On


neighbours : List ( Int, Int )
neighbours =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


aliveNeighbourCountOfCellAtRC : Int -> Int -> Grid -> Int
aliveNeighbourCountOfCellAtRC rowNum colNum grid =
    neighbours
        |> List.foldl
            (\( dr, dc ) ct ->
                case Matrix.getWarped (rowNum + dr) (colNum + dc) grid of
                    Just On ->
                        ct + 1

                    _ ->
                        ct
            )
            0


{-| <https://www.conwaylife.com/wiki/Conway's_Game_of_Life>
-}
nextStateOfCell : Int -> Cell -> Cell
nextStateOfCell aliveNeighbourCount cell =
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


nextState : Grid -> Grid
nextState grid =
    Matrix.indexedMap
        (\rowNum colNum ->
            nextStateOfCell (aliveNeighbourCountOfCellAtRC rowNum colNum grid)
        )
        grid


initEmpty : { a | colCount : Int, rowCount : Int } -> Grid
initEmpty { rowCount, colCount } =
    Matrix.repeat rowCount colCount Off


randomize : Grid -> Generator Grid
randomize grid =
    let
        randomGridCell : Generator Cell
        randomGridCell =
            Random.weighted ( 80, Off ) [ ( 20, On ) ]

        ( rowCount, colCount ) =
            Matrix.size grid
    in
    Matrix.generator rowCount colCount randomGridCell


indexedMapToList : (Int -> Int -> Cell -> a) -> Grid -> List a
indexedMapToList func =
    Matrix.indexedMap func >> Matrix.toList
