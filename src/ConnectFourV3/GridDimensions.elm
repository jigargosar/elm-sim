module ConnectFourV3.GridDimensions exposing
    ( GridDimensions
    , clampColoumn
    , contains
    , foldl
    , fromColumnsRows
    , size
    , toColoumnsRows
    )


type GridDimensions
    = GridDimensions { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


fromColumnsRows : { a | columns : Int, rows : Int } -> GridDimensions
fromColumnsRows { columns, rows } =
    GridDimensions { columns = columns, rows = rows }


toColoumnsRows : GridDimensions -> { columns : Int, rows : Int }
toColoumnsRows (GridDimensions rec) =
    rec


foldl : (Position -> c -> c) -> c -> GridDimensions -> c
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


contains : Position -> GridDimensions -> Bool
contains ( x, y ) (GridDimensions { columns, rows }) =
    (x < 0 || y < 0 || x >= columns || y >= columns)
        |> not


size : GridDimensions -> Int
size (GridDimensions { columns, rows }) =
    columns * rows


clampColoumn : Int -> GridDimensions -> Int
clampColoumn column (GridDimensions { columns }) =
    clamp 0 (columns - 1) column
