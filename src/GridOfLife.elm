module GridOfLife exposing (Cell(..), Grid, asList2d, initEmpty, nextState, randomize, toggleCellAtRC)

import Array exposing (Array)
import Random exposing (Generator)


type Cell
    = On
    | Off


type alias Row =
    Array Cell


type alias Rows =
    Array Row


type alias Grid =
    { rowCount : Int
    , colCount : Int
    , rows : Rows
    }


mapRows : (Rows -> Rows) -> Grid -> Grid
mapRows func grid =
    { grid | rows = func grid.rows }


cellAtRC : Int -> Int -> Grid -> Cell
cellAtRC rowNum_ colNum_ grid =
    let
        rowNum =
            modBy grid.rowCount rowNum_

        colNum =
            modBy grid.colCount colNum_
    in
    Array.get rowNum grid.rows
        |> Maybe.andThen (Array.get colNum)
        |> Maybe.withDefault Off


toggleCellAtRC : Int -> Int -> Grid -> Grid
toggleCellAtRC rowNum colNum grid =
    let
        toggleCell cell =
            case cell of
                On ->
                    Off

                Off ->
                    On
    in
    mapCellAt rowNum colNum toggleCell grid


mapCellAt : Int -> Int -> (Cell -> Cell) -> Grid -> Grid
mapCellAt rowNum colNum_ func grid =
    let
        colNum =
            modBy grid.colCount colNum_
    in
    mapRowAt rowNum (arrayMapAt colNum func) grid


mapRowAt : Int -> (Row -> Row) -> Grid -> Grid
mapRowAt rowNum_ func grid =
    let
        rowNum =
            modBy grid.rowCount rowNum_
    in
    mapRows (arrayMapAt rowNum func) grid


arrayMapAt : Int -> (a -> a) -> Array a -> Array a
arrayMapAt idx func arr =
    Array.get idx arr
        |> Maybe.map (func >> (\v -> Array.set idx v arr))
        |> Maybe.withDefault arr


neighbours : List ( Int, Int )
neighbours =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


neighboursOfCellAtRC : Int -> Int -> Grid -> List Cell
neighboursOfCellAtRC rowNum colNum grid =
    neighbours
        |> List.map (\( dr, dc ) -> cellAtRC (rowNum + dr) (colNum + dc) grid)


is : a -> a -> Bool
is =
    (==)


aliveNeighbourCountOfCellAtRC : Int -> Int -> Grid -> Int
aliveNeighbourCountOfCellAtRC row col grid =
    neighboursOfCellAtRC row col grid
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
initEmpty { rowCount, colCount } =
    { rowCount = rowCount
    , colCount = colCount
    , rows = Array.repeat colCount Off |> Array.repeat rowCount
    }


randomize : Grid -> Generator Grid
randomize { rowCount, colCount } =
    let
        randomGridCell : Generator Cell
        randomGridCell =
            Random.weighted ( 80, Off ) [ ( 20, On ) ]

        randomGridRow : Generator (Array Cell)
        randomGridRow =
            randomArray colCount randomGridCell
    in
    randomArray rowCount randomGridRow
        |> Random.map
            (\rows ->
                { rowCount = rowCount
                , colCount = colCount
                , rows = rows
                }
            )


randomArray : Int -> Generator a -> Generator (Array a)
randomArray count =
    Random.list count >> Random.map Array.fromList


asList2d : Grid -> List (List Cell)
asList2d =
    .rows >> Array.map Array.toList >> Array.toList
