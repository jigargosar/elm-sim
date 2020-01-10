module ConnectFourV3Kata1.Grid exposing (Grid, allPositions, centerX, empty)

import Dict exposing (Dict)


type alias Dim =
    { width : Int
    , height : Int
    }


type alias Pos =
    ( Int, Int )


type Grid a
    = Grid Dim (Dict Pos a)


empty : { a | width : Int, height : Int } -> Grid v
empty rec =
    Grid (Dim rec.width rec.height) Dict.empty


centerX : Grid a -> Int
centerX grid =
    width grid // 2


allPositions : Grid a -> List Pos
allPositions g =
    List.range 0 (height g - 1)
        |> List.concatMap
            (\y -> List.range 0 (width g - 1) |> List.map (\x -> toPos x y))


toPos : Int -> Int -> Pos
toPos =
    Tuple.pair


width : Grid a -> Int
width =
    toDim >> .width


height : Grid a -> Int
height =
    toDim >> .height


toDim : Grid a -> Dim
toDim (Grid dim _) =
    dim
