module GridOfLife exposing (Cell(..), Grid, asList2d, cellAtRC, generator, initEmpty, nextState)

import Array exposing (Array)
import Random exposing (Generator)


type Cell
    = On
    | Off


type alias GridRow =
    Array Cell


type alias Grid =
    { rows : Array GridRow }


cellAtRC : Int -> Int -> Grid -> Cell
cellAtRC row col grid =
    Array.get row grid.rows
        |> Maybe.andThen (Array.get col)
        |> Maybe.withDefault Off


neighbours : List ( Int, Int )
neighbours =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


neighboursOfRC : Int -> Int -> Grid -> List Cell
neighboursOfRC row col grid =
    neighbours
        |> List.map (\( nr, nc ) -> cellAtRC (row + nr) (col + nc) grid)


is : a -> a -> Bool
is =
    (==)


aliveNeighbourCountOfCellAtRC : Int -> Int -> Grid -> Int
aliveNeighbourCountOfCellAtRC row col grid =
    neighboursOfRC row col grid
        |> List.filter (is On)
        |> List.length


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


mapRows : (b -> b) -> { a | rows : b } -> { a | rows : b }
mapRows func model =
    { model | rows = func model.rows }


nextState : Grid -> Grid
nextState grid =
    let
        func =
            mapRCArrayIndexed
                (\rowNum colNum ->
                    nextStateOfCell (aliveNeighbourCountOfCellAtRC rowNum colNum grid)
                )
    in
    mapRows func grid


mapRCArrayIndexed : (Int -> Int -> a -> b) -> Array (Array a) -> Array (Array b)
mapRCArrayIndexed func =
    Array.indexedMap (func >> Array.indexedMap)


initEmpty : { a | colCount : Int, rowCount : Int } -> Grid
initEmpty gridConfig =
    { rows =
        Array.repeat gridConfig.colCount Off
            |> Array.repeat gridConfig.rowCount
    }


randomArray : Int -> Generator a -> Generator (Array a)
randomArray count =
    Random.list count >> Random.map Array.fromList


generator : { a | colCount : Int, rowCount : Int } -> Generator Grid
generator gridConfig =
    let
        randomGridCell : Generator Cell
        randomGridCell =
            Random.weighted ( 80, Off ) [ ( 20, On ) ]

        randomGridRow : Generator (Array Cell)
        randomGridRow =
            randomArray gridConfig.colCount randomGridCell
    in
    randomArray gridConfig.rowCount randomGridRow
        |> Random.map (\rows -> { rows = rows })


asList2d : Grid -> List (List Cell)
asList2d =
    .rows >> Array.map Array.toList >> Array.toList
