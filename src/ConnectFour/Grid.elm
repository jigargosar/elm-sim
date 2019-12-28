module ConnectFour.Grid exposing
    ( Cell
    , Coin(..)
    , Grid
    , Position
    , clampPosition
    , dimensions
    , empty
    , emptyPositions
    , firstEmptyPositionInColumn
    , setFirstEmptyCellInColumn
    , toCellList
    )

import Grid.Bordered as Grid
import List.Extra
import PointFree exposing (flip)


type Coin
    = Red
    | Yellow


type alias Cell =
    Maybe Coin


type Grid
    = Grid GridModel


type alias GridModel =
    { width : Int
    , height : Int
    , grid : Grid.Grid Coin
    }


type alias Position =
    ( Int, Int )


unwrap : Grid -> GridModel
unwrap (Grid grid) =
    grid


clampPosition : Grid -> Position -> Position
clampPosition =
    unwrap >> (\grid -> Tuple.mapBoth (clamp 0 (grid.width - 1)) (clamp 0 (grid.height - 1)))


empty : Int -> Int -> Grid
empty w h =
    Grid { width = w, height = h, grid = Grid.empty { columns = w, rows = h } }


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


setGrid : Grid.Grid Coin -> Grid -> Grid
setGrid grid =
    map <| \model -> { model | grid = grid }


setFirstEmptyCellInColumn : Int -> Coin -> Grid -> Grid
setFirstEmptyCellInColumn column coin model =
    case firstEmptyPositionInColumn column model of
        Just cord ->
            case insert cord coin model of
                Ok newModel ->
                    newModel

                Err _ ->
                    model

        Nothing ->
            model


insert : Position -> Coin -> Grid -> Result Grid.Error Grid
insert position coin model =
    let
        grid =
            unwrap model |> .grid
    in
    Grid.insert position coin grid |> Result.map (flip setGrid model)


columnEq : Int -> Position -> Bool
columnEq value ( column, _ ) =
    value == column


firstEmptyPositionInColumn : Int -> Grid -> Maybe Position
firstEmptyPositionInColumn column =
    emptyPositions >> List.Extra.find (columnEq column)


emptyPositions : Grid -> List Position
emptyPositions =
    unwrap >> .grid >> Grid.emptyPositions


toCellList : Grid -> List ( Position, Cell )
toCellList =
    unwrap >> .grid >> Grid.foldl (\p c -> (::) ( p, c )) []


dimensions : Grid -> Position
dimensions =
    unwrap >> (\{ width, height } -> ( width, height ))
