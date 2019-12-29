module ConnectFour.Grid exposing
    ( Cell
    , Coin(..)
    , Grid
    , Position
    , clampPosition
    , dimensions
    , empty
    , firstEmptyPositionInColumn
    , putCoinInColumn
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
    Grid.Grid Coin


type alias Position =
    ( Int, Int )


unwrap : Grid -> GridModel
unwrap (Grid grid) =
    grid


clampPosition : Grid -> Position -> Position
clampPosition =
    dimensions >> (\{ width, height } -> Tuple.mapBoth (clamp 0 (width - 1)) (clamp 0 (height - 1)))


empty : Int -> Int -> Grid
empty w h =
    Grid.empty { columns = w, rows = h } |> Grid


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


type Error
    = OutOfBounds
    | NotSuccessful


convertError : Grid.Error -> Error
convertError error =
    case error of
        Grid.OutOfBounds ->
            OutOfBounds

        Grid.NotSuccessful ->
            NotSuccessful


putCoinInColumn : Int -> Coin -> Grid -> Result Error Grid
putCoinInColumn column coin model =
    firstEmptyPositionInColumn column model
        |> Maybe.map
            (\position ->
                insert position coin model
                    |> Result.mapError convertError
            )
        |> Maybe.withDefault (Err NotSuccessful)


insert : Position -> Coin -> Grid -> Result Grid.Error Grid
insert position coin model =
    let
        grid =
            unwrap model
    in
    Grid.insert position coin grid |> Result.map Grid


columnEq : Int -> Position -> Bool
columnEq value ( column, _ ) =
    value == column


firstEmptyPositionInColumn : Int -> Grid -> Maybe Position
firstEmptyPositionInColumn column =
    unwrap >> Grid.emptyPositions >> List.Extra.find (columnEq column)


toCellList : Grid -> List ( Position, Cell )
toCellList =
    unwrap >> Grid.foldl (\p c -> (::) ( p, c )) []


dimensions : Grid -> { width : Int, height : Int }
dimensions =
    unwrap >> Grid.dimensions >> (\{ columns, rows } -> { width = columns, height = rows })
