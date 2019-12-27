module ConnectFour.Grid exposing
    ( Cell(..)
    , Cord
    , Grid
    , clampCord
    , dimensions
    , empty
    , getFirstNonEmptyCordWhereXEq
    , setFirstNonEmptyYOfX
    , toList
    )

import Dict exposing (Dict)
import Dict.Extra
import PointFree exposing (pairTo)


type Cell
    = Red
    | Yellow
    | Empty


type Grid
    = Grid GridModel


type alias GridModel =
    { width : Int
    , height : Int
    , cells : Dict Cord Cell
    }


type alias Cord =
    ( Int, Int )


unwrap : Grid -> GridModel
unwrap (Grid grid) =
    grid


clampCord : Grid -> Cord -> Cord
clampCord =
    unwrap >> (\grid -> Tuple.mapBoth (clamp 0 (grid.width - 1)) (clamp 0 (grid.height - 1)))


empty : Int -> Int -> Grid
empty w h =
    let
        cords : List Cord
        cords =
            List.range 0 (w - 1)
                |> List.map (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
                |> List.concat
                |> List.sort

        emptyCells =
            List.map (pairTo Empty) cords |> Dict.fromList
    in
    Grid { width = w, height = h, cells = emptyCells }


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


update : Cord -> (Cell -> Cell) -> Grid -> Grid
update cord func =
    map <| \grid -> { grid | cells = Dict.update cord (Maybe.map func) grid.cells }


xEq : Int -> Cord -> Bool
xEq column ( x, _ ) =
    column == x


setFirstNonEmptyYOfX : Int -> Cell -> Grid -> Grid
setFirstNonEmptyYOfX x cell grid =
    case getFirstNonEmptyCordWhereXEq x grid of
        Just cord ->
            update cord (always cell) grid

        Nothing ->
            grid


getFirstNonEmptyCordWhereXEq : Int -> Grid -> Maybe Cord
getFirstNonEmptyCordWhereXEq x =
    let
        pred cord cell =
            xEq x cord && cell == Empty
    in
    find pred >> Maybe.map Tuple.first


find : (Cord -> Cell -> Bool) -> Grid -> Maybe ( Cord, Cell )
find pred =
    unwrap >> .cells >> Dict.Extra.find pred


toList : Grid -> List ( Cord, Cell )
toList =
    unwrap >> .cells >> Dict.toList


dimensions : Grid -> Cord
dimensions =
    unwrap >> (\{ width, height } -> ( width, height ))
