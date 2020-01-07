module ConnectFourV3.GridDimensions exposing (contains, foldl, size)


type alias GridDimensions =
    { columns : Int, rows : Int }


type alias Position =
    ( Int, Int )


foldl : (Position -> c -> c) -> c -> GridDimensions -> c
foldl func acc0 { columns, rows } =
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
contains ( x, y ) { columns, rows } =
    (x < 0 || y < 0 || x >= columns || y >= columns)
        |> not


size : GridDimensions -> Int
size { columns, rows } =
    columns * rows
