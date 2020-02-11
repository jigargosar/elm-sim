module ConnectFourV3Kata1.Grid exposing (centerX, positions)

import Dict exposing (Dict)


type alias Dim =
    { width : Int
    , height : Int
    }


type alias Pos =
    ( Int, Int )


centerX : Dim -> Int
centerX { width } =
    width // 2


positions : Dim -> List Pos
positions { width, height } =
    List.range 0 (height - 1)
        |> List.concatMap
            (\y -> List.range 0 (width - 1) |> List.map (\x -> ( x, y )))
