module ConnectFour.Grid exposing
    ( Cell(..)
    , Grid
    , empty
    , get
    , mapCellAt
    , setCellAt
    , validateGridCord
    )

import Dict exposing (Dict)


type Cell
    = Red
    | Yellow
    | Empty


type alias Grid =
    { width : Int
    , height : Int
    , cords : List ( Int, Int )
    , cells : Dict ( Int, Int ) Cell
    }


empty : Int -> Int -> Grid
empty w h =
    let
        cords =
            List.range 0 (w - 1)
                |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
                |> List.concat
                |> List.sort
    in
    { width = w, height = h, cords = cords, cells = Dict.empty }


get : ( Int, Int ) -> Grid -> Maybe Cell
get cord grid =
    if isValidGridCord cord grid then
        Just (Dict.get cord grid.cells |> Maybe.withDefault Empty)

    else
        Nothing


setCellAt : ( Int, Int ) -> Cell -> Grid -> Grid
setCellAt cord cell grid =
    { grid | cells = Dict.insert cord cell grid.cells }


mapCellAt : ( Int, Int ) -> (Cell -> Cell) -> Grid -> Grid
mapCellAt cord func grid =
    case get cord grid of
        Just cell ->
            setCellAt cord (func cell) grid

        Nothing ->
            grid


validateGridCord : ( Int, Int ) -> Grid -> Maybe ( Int, Int )
validateGridCord cord grid =
    if isValidGridCord cord grid then
        Just cord

    else
        Nothing


isValidGridCord : ( Int, Int ) -> Grid -> Bool
isValidGridCord ( x, y ) grid =
    let
        isInvalid =
            x < 0 || y < 0 || x >= grid.width || y >= grid.height
    in
    not isInvalid
