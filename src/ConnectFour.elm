module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell(..), Grid)
import Playground exposing (..)
import PointFree exposing (flip)


initialMem : Grid
initialMem =
    Grid.empty 10 18
        |> Grid.setCellAt ( 0, 0 ) Yellow
        |> Grid.setCellAt ( 0, 1 ) Red
        |> Grid.setCellAt ( 0, 2 ) Yellow
        |> Grid.setCellAt ( 7, 0 ) Yellow


update : Computer -> Grid -> Grid
update { mouse } mem =
    let
        gvm =
            toGridViewModel mem

        clickedGridCord =
            if mouse.click then
                screenCordToGridCord ( mouse.x, mouse.y ) gvm

            else
                Nothing

        _ =
            if mouse.click then
                Debug.log "mouseGridCord" clickedGridCord

            else
                clickedGridCord
    in
    case clickedGridCord of
        Just a ->
            cycleCellAt a mem

        Nothing ->
            mem


cycleCellAt : ( Int, Int ) -> Grid -> Grid
cycleCellAt cord =
    Grid.update cord cycleCell


cycleCell : Cell -> Cell
cycleCell cell =
    case cell of
        Empty ->
            Red

        Red ->
            Yellow

        Yellow ->
            Empty


screenCordToGridCord : ( Float, Float ) -> GridViewModel -> Maybe ( Int, Int )
screenCordToGridCord ( x, y ) gvm =
    let
        gridCord =
            ( (x - gvm.left) / gvm.cellSize |> round, (y - gvm.bottom) / gvm.cellSize |> round )
    in
    Grid.validateGridCord gridCord gvm.grid


view _ grid =
    [ viewGrid grid ]


cellColor : Cell -> Color
cellColor cell =
    case cell of
        Empty ->
            white

        Red ->
            red

        Yellow ->
            yellow


gridCordToScreenCord : GridViewModel -> ( Int, Int ) -> ( Float, Float )
gridCordToScreenCord gvm ( x, y ) =
    ( gvm.left + toFloat x * gvm.cellSize, gvm.bottom + toFloat y * gvm.cellSize )


viewGridCellAt : ( Int, Int ) -> GridViewModel -> Maybe Shape
viewGridCellAt cord gvm =
    let
        ( x, y ) =
            gridCordToScreenCord gvm cord

        func cell =
            circle (cellColor cell) gvm.cellRadius
                |> move x y
    in
    Grid.get cord gvm.grid
        |> Maybe.map func


viewGrid : Grid -> Shape
viewGrid grid =
    let
        gvm =
            toGridViewModel grid

        off =
            gvm.cellSize
    in
    group
        [ rectangle blue (gvm.width + off) (gvm.height + off)
        , List.filterMap (flip viewGridCellAt gvm) grid.cords |> group
        ]


type alias GridViewModel =
    { width : Float
    , height : Float
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
    , cellSize : Number
    , cellRadius : Number
    , grid : Grid
    }


toGridViewModel : Grid -> GridViewModel
toGridViewModel grid =
    let
        cellSize =
            50

        width =
            toFloat (grid.width - 1) * cellSize

        height =
            toFloat (grid.height - 1) * cellSize
    in
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    , cellSize = cellSize
    , cellRadius = cellSize / 2 - cellSize / 10
    , grid = grid
    }


main =
    game view update initialMem
