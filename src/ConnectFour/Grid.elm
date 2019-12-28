module ConnectFour.Grid exposing
    ( Cell
    , Coin(..)
    , Cord
    , Grid
    , clampCord
    , dimensions
    , empty
    , emptyPositions
    , getFirstEmptyCordWhereXEq
    , setFirstEmptyYOfX
    , toCellList
    )

import Grid.Bordered as Grid
import Grid.Position exposing (Position)
import List.Extra


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
    Grid { width = w, height = h, grid = Grid.empty { columns = w, rows = h } }


map : (GridModel -> GridModel) -> Grid -> Grid
map func =
    unwrap >> func >> Grid


xEq : Int -> Cord -> Bool
xEq column ( x, _ ) =
    column == x


setGrid : Grid.Grid Coin -> Grid -> Grid
setGrid grid =
    map <| \model -> { model | grid = grid }


setFirstEmptyYOfX : Int -> Coin -> Grid -> Grid
setFirstEmptyYOfX x coin model =
    let
        grid =
            unwrap model |> .grid
    in
    case getFirstEmptyCordWhereXEq x model of
        Just cord ->
            case Grid.insert cord coin grid of
                Ok value ->
                    setGrid value model

                Err _ ->
                    model

        Nothing ->
            model


getFirstEmptyCordWhereXEq : Int -> Grid -> Maybe Cord
getFirstEmptyCordWhereXEq x =
    emptyPositions >> List.Extra.find (xEq x)


emptyPositions : Grid -> List Cord
emptyPositions =
    unwrap >> .grid >> Grid.emptyPositions


toCellList : Grid -> List ( Position, Cell )
toCellList =
    unwrap >> .grid >> Grid.foldl (\p c -> (::) ( p, c )) []


dimensions : Grid -> Cord
dimensions =
    unwrap >> (\{ width, height } -> ( width, height ))
