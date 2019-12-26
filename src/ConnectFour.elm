module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell(..), Grid)
import Playground exposing (..)
import PointFree exposing (flip)


type alias Mem =
    { grid : Grid, turn : Cell }


initialMem : Mem
initialMem =
    { grid = initialGrid, turn = Red }


initialGrid : Grid
initialGrid =
    Grid.empty 10 18
        |> Grid.set ( 0, 0 ) Yellow
        |> Grid.set ( 0, 1 ) Red
        |> Grid.set ( 0, 2 ) Yellow
        |> Grid.set ( 7, 0 ) Yellow


update : Computer -> Mem -> Mem
update { mouse } mem =
    let
        gvm =
            toGridViewModel mem.grid
    in
    if mouse.click then
        { mem | grid = fillGridColumn (screenCordToGridCord ( mouse.x, mouse.y ) gvm) mem.grid }

    else
        mem


fillGridColumn : ( Int, Int ) -> Grid -> Grid
fillGridColumn ( x, _ ) =
    Grid.setAtFirstNonEmptyYOfX x Red


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



{-
   screenCordToValidatedGridCord : ( Float, Float ) -> GridViewModel -> Maybe ( Int, Int )
   screenCordToValidatedGridCord screenCord gvm =
       Grid.validateGridCord (screenCordToGridCord screenCord gvm) gvm.grid
-}


screenCordToGridCord : ( Float, Float ) -> GridViewModel -> ( Int, Int )
screenCordToGridCord ( x, y ) gvm =
    ( (x - gvm.left) / gvm.cellSize |> round, (y - gvm.bottom) / gvm.cellSize |> round )


view : Computer -> Mem -> List Shape
view _ mem =
    [ viewGrid mem.grid ]


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
