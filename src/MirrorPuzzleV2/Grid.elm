module MirrorPuzzleV2.Grid exposing (Grid, Pos, filled)

import Dict exposing (Dict)


type alias Pos =
    ( Int, Int )


type Grid a
    = Grid Int Int (Dict Pos a)


filled : Int -> Int -> a -> Grid a
filled w h a =
    foldWH (\pos -> Dict.insert pos a) Dict.empty w h |> Grid w h


foldWH : (Pos -> b -> b) -> b -> Int -> Int -> b
foldWH func acc0 w h =
    List.range 0 (h - 1)
        |> List.foldl
            (\y acc1 ->
                List.range 0 (w - 1)
                    |> List.foldl (\x -> func ( x, y )) acc1
            )
            acc0
