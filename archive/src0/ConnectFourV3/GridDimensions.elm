module ConnectFourV3.GridDimensions exposing
    ( GridDimensions
    , centerColumn
    , clampColoumn
    , contains
    , containsColumn
    , foldl
    , fromColumnsRows
    , lastRow
    , neighboursOffset
    , size
    , stepPositionBy
    , toColoumnsRows
    )

import ConnectFourV3.GridPosition exposing (GridPosition)


type GridDimensions
    = GridDimensions { columns : Int, rows : Int }


fromColumnsRows : { a | columns : Int, rows : Int } -> GridDimensions
fromColumnsRows { columns, rows } =
    GridDimensions { columns = columns, rows = rows }


toColoumnsRows : GridDimensions -> { columns : Int, rows : Int }
toColoumnsRows (GridDimensions rec) =
    rec


foldl : (GridPosition -> c -> c) -> c -> GridDimensions -> c
foldl func acc0 (GridDimensions { columns, rows }) =
    List.range 0 (columns - 1)
        |> List.foldl
            (\column acc1 ->
                List.range 0 (rows - 1)
                    |> List.foldl
                        (\row ->
                            let
                                position =
                                    ( column, row )
                            in
                            func position
                        )
                        acc1
            )
            acc0


contains : GridPosition -> GridDimensions -> Bool
contains ( x, y ) (GridDimensions { columns, rows }) =
    (x < 0 || y < 0 || x >= columns || y >= rows)
        |> not


containsColumn : Int -> GridDimensions -> Bool
containsColumn column (GridDimensions { columns }) =
    (column < 0 || column >= columns)
        |> not


size : GridDimensions -> Int
size (GridDimensions { columns, rows }) =
    columns * rows


clampColoumn : Int -> GridDimensions -> Int
clampColoumn column (GridDimensions { columns }) =
    clamp 0 (columns - 1) column


centerColumn : GridDimensions -> Int
centerColumn (GridDimensions { columns }) =
    columns // 2


stepPositionBy : ( Int, Int ) -> GridDimensions -> GridPosition -> Maybe GridPosition
stepPositionBy ( dx, dy ) dim ( x, y ) =
    let
        nextPosition =
            ( x + dx, y + dy )
    in
    if contains nextPosition dim then
        Just nextPosition

    else
        Nothing


neighboursOffset : List ( Int, Int )
neighboursOffset =
    [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]


lastRow : GridDimensions -> Int
lastRow (GridDimensions { rows }) =
    rows - 1
