module ConnectFour.Grid exposing
    ( Cell(..)
    , Cord
    , Grid
    , clampCord
    , dimensions
    , empty
    , getFirstNonEmptyCordWhereXEq
    , set
    , setAtFirstNonEmptyYOfX
    , toList
    )

import Dict exposing (Dict)
import Dict.Extra


type Cell
    = Red
    | Yellow
    | Empty


type Grid
    = Grid GridModel


type alias GridModel =
    { width : Int
    , height : Int
    , cords : List Cord
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

        setEmpty : Cord -> Dict Cord Cell -> Dict Cord Cell
        setEmpty cord =
            Dict.insert cord Empty
    in
    Grid { width = w, height = h, cords = cords, cells = List.foldl setEmpty Dict.empty cords }


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


set : Cord -> Cell -> Grid -> Grid
set cord cell =
    update cord (always cell)


update : Cord -> (Cell -> Cell) -> Grid -> Grid
update cord func =
    map <| \grid -> { grid | cells = Dict.update cord (Maybe.map func) grid.cells }


xEq : Int -> Cord -> Bool
xEq column ( x, _ ) =
    column == x


setAtFirstNonEmptyYOfX : Int -> Cell -> Grid -> Grid
setAtFirstNonEmptyYOfX x cell grid =
    case getFirstNonEmptyCordWhereXEq x grid of
        Just cord ->
            set cord cell grid

        Nothing ->
            grid


getFirstNonEmptyCordWhereXEq : Int -> Grid -> Maybe Cord
getFirstNonEmptyCordWhereXEq x grid =
    let
        pred cord cellAtCord =
            xEq x cord && cellAtCord == Empty
    in
    findCord pred grid


find : (( Int, Int ) -> Cell -> Bool) -> Grid -> Maybe ( Cord, Cell )
find pred =
    unwrap >> .cells >> Dict.Extra.find pred


findCord : (( Int, Int ) -> Cell -> Bool) -> Grid -> Maybe Cord
findCord pred grid =
    find pred grid |> Maybe.map Tuple.first


toList : Grid -> List ( Cord, Cell )
toList =
    unwrap >> .cells >> Dict.toList


dimensions : Grid -> Cord
dimensions =
    unwrap >> (\{ width, height } -> ( width, height ))
