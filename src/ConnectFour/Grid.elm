module ConnectFour.Grid exposing
    ( Cell(..)
    , Grid
    , empty
    , get
    , set
    , update
    , validateGridCord
    )

import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import PointFree exposing (flip)


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
        cords : List ( Int, Int )
        cords =
            List.range 0 (w - 1)
                |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
                |> List.concat
                |> List.sort

        setEmpty : ( Int, Int ) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
        setEmpty cord =
            Dict.insert cord Empty
    in
    { width = w, height = h, cords = cords, cells = List.foldl setEmpty Dict.empty cords }


get : ( Int, Int ) -> Grid -> Maybe Cell
get cord grid =
    if isValidGridCord cord grid then
        Dict.get cord grid.cells

    else
        Nothing


set : ( Int, Int ) -> Cell -> Grid -> Grid
set cord cell grid =
    { grid | cells = Dict.insert cord cell grid.cells }


update : ( Int, Int ) -> (Cell -> Cell) -> Grid -> Grid
update cord func grid =
    { grid | cells = Dict.update cord (Maybe.map func) grid.cells }


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


columnEq : Int -> ( Int, Int ) -> Bool
columnEq column ( x, _ ) =
    column == x


isCellEmptyAt : ( Int, Int ) -> Grid -> Bool
isCellEmptyAt cord grid =
    get cord grid |> Maybe.map ((==) Empty) |> Maybe.withDefault True


setAtFirstNonEmptyRowOfColumn : Int -> Cell -> Grid -> Grid
setAtFirstNonEmptyRowOfColumn column cell grid =
    let
        pred cord cellAtCord =
            columnEq column cord && cellAtCord == Empty
    in
    case find pred grid of
        Just ( cord, _ ) ->
            set cord cell grid

        Nothing ->
            grid


find : (( Int, Int ) -> Cell -> Bool) -> Grid -> Maybe ( ( Int, Int ), Cell )
find pred grid =
    Dict.Extra.find pred grid.cells
