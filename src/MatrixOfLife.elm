module MatrixOfLife exposing
    ( Cell(..)
    , Grid
    , indexedMapToList
    , initEmpty
    , nextState
    , randomize
    , toggleCellAtRC
    )

import Array exposing (Array)
import Matrix exposing (Matrix)
import Random exposing (Generator)


type Cell
    = On
    | Off


type alias Row =
    Array Cell


type alias Rows =
    Matrix Cell


type alias Grid =
    { rowCount : Int
    , colCount : Int
    , rows : Rows
    }


cellAtRC : Int -> Int -> Grid -> Cell
cellAtRC rowNum_ colNum_ grid =
    Matrix.getWarped rowNum_ colNum_ grid.rows
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
mapCellAt rowNum colNum func grid =
    { grid | rows = Matrix.mapAt rowNum colNum func grid.rows }


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
            Matrix.indexedMap
                (\rowNum colNum ->
                    nextStateOfCell (aliveNeighbourCountOfCellAtRC rowNum colNum grid)
                )
    in
    { grid | rows = func grid.rows }


mapRCArrayIndexed : (Int -> Int -> a -> b) -> Array (Array a) -> Array (Array b)
mapRCArrayIndexed func =
    Array.indexedMap (func >> Array.indexedMap)


initEmpty : { a | colCount : Int, rowCount : Int } -> Grid
initEmpty { rowCount, colCount } =
    Grid rowCount colCount (Matrix.repeat rowCount colCount Off)


randomize : Grid -> Generator Grid
randomize { rowCount, colCount } =
    let
        randomGridCell : Generator Cell
        randomGridCell =
            Random.weighted ( 80, Off ) [ ( 20, On ) ]
    in
    Matrix.generator rowCount colCount randomGridCell
        |> Random.map (Grid rowCount colCount)


randomArray : Int -> Generator a -> Generator (Array a)
randomArray count =
    Random.list count >> Random.map Array.fromList


asGrid : Grid -> Grid
asGrid =
    identity


indexedMapToList : (Int -> Int -> Cell -> a) -> Grid -> List a
indexedMapToList func =
    asGrid
        >> .rows
        >> Matrix.indexedMap func
        >> Matrix.toList
