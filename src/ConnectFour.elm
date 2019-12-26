module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell(..), Grid)
import Playground exposing (..)
import PointFree exposing (flip)


type alias Mem =
    { grid : Grid
    , currentPlayer : Player
    }


type Player
    = PlayerRed
    | PlayerYellow


initialMem : Mem
initialMem =
    { grid = initialGrid, currentPlayer = PlayerRed }


initialGrid : Grid
initialGrid =
    Grid.empty 6 5
        |> Grid.set ( 0, 0 ) Yellow
        |> Grid.set ( 0, 1 ) Red
        |> Grid.set ( 0, 2 ) Yellow
        |> Grid.set ( 7, 0 ) Yellow


update : Computer -> Mem -> Mem
update { mouse, screen } mem =
    if mouse.click then
        let
            gvm =
                toGridViewModel screen mem.grid

            ( x, _ ) =
                screenCordToGridCord ( mouse.x, mouse.y ) gvm

            newGrid =
                Grid.setAtFirstNonEmptyYOfX x (playerToCell mem.currentPlayer) mem.grid
        in
        { mem
            | grid = newGrid
            , currentPlayer =
                if newGrid == mem.grid then
                    mem.currentPlayer

                else
                    swapPlayer mem.currentPlayer
        }

    else
        mem


swapPlayer : Player -> Player
swapPlayer cell =
    case cell of
        PlayerRed ->
            PlayerYellow

        PlayerYellow ->
            PlayerRed


playerToCell : Player -> Cell
playerToCell player =
    case player of
        PlayerRed ->
            Red

        PlayerYellow ->
            Yellow


screenCordToGridCord : ( Float, Float ) -> GridViewModel -> ( Int, Int )
screenCordToGridCord ( x, y ) gvm =
    ( (x - gvm.left) / gvm.cellSize |> round, (y - gvm.bottom) / gvm.cellSize |> round )


view : Computer -> Mem -> List Shape
view { screen } mem =
    [ viewGrid screen mem.grid ]


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


viewGrid : Screen -> Grid -> Shape
viewGrid screen grid =
    let
        gvm =
            toGridViewModel screen grid

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


toGridViewModel : Screen -> Grid -> GridViewModel
toGridViewModel screen grid =
    let
        cellWidth =
            (screen.width * 0.9) / toFloat grid.width

        cellHeight =
            (screen.height * 0.9) / toFloat grid.height

        cellSize =
            min cellWidth cellHeight |> round |> toFloat

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
