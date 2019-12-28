module ConnectFour.Grid exposing
    ( Cell
    , Coin(..)
    , Grid
    , Position
    , clampPosition
    , dimensionsToTuple
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
    { grid : Grid.Grid Coin
    }


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
    Grid { grid = Grid.empty { columns = w, rows = h } }


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


setGrid : Grid.Grid Coin -> Grid -> Grid
setGrid grid =
    map <| \model -> { model | grid = grid }


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


dimensionsToTuple : Grid -> ( Int, Int )
dimensionsToTuple =
    unwrap >> .grid >> Grid.dimensions >> (\{ columns, rows } -> ( columns, rows ))


dimensions : Grid -> { width : Int, height : Int }
dimensions =
    unwrap >> .grid >> Grid.dimensions >> (\{ columns, rows } -> { width = columns, height = rows })
