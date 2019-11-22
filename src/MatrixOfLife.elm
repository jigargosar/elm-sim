module MatrixOfLife exposing
    ( Grid
    , indexedMapToList
    , initEmpty
    , nextState
    , randomize
    , toggleCellAtRC
    )

import GameOfLife.Cell as Cell exposing (Cell)
import Matrix exposing (Matrix)
import Random exposing (Generator)


type alias Grid =
    Matrix Cell


toggleCellAtRC : Int -> Int -> Grid -> Grid
toggleCellAtRC rowNum colNum =
    Matrix.mapAt rowNum colNum Cell.toggle


nextState : Grid -> Grid
nextState grid =
    let
        cellAt : Int -> Int -> Maybe Cell
        cellAt ri ci =
            Matrix.getWarped ri ci grid
    in
    Matrix.indexedMap
        (\rowNum colNum ->
            Cell.nextState rowNum colNum cellAt
        )
        grid


initEmpty : { a | colCount : Int, rowCount : Int } -> Grid
initEmpty { rowCount, colCount } =
    Matrix.repeat rowCount colCount Cell.off


randomize : Grid -> Generator Grid
randomize grid =
    let
        randomGridCell : Generator Cell
        randomGridCell =
            Random.weighted ( 80, Cell.off ) [ ( 20, Cell.on ) ]

        ( rowCount, colCount ) =
            Matrix.size grid
    in
    Matrix.generator rowCount colCount randomGridCell


indexedMapToList : (Int -> Int -> Cell -> a) -> Grid -> List a
indexedMapToList func =
    Matrix.indexedMap func >> Matrix.toList
