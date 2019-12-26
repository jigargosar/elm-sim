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


screenCordToGridCord : ScreenCord -> GridViewModel -> Grid.Cord
screenCordToGridCord ( x, y ) gvm =
    ( (x - gvm.left) / gvm.cellSize |> round, (y - gvm.bottom) / gvm.cellSize |> round )


view : Computer -> Mem -> List Shape
view computer mem =
    [ viewGrid computer mem.currentPlayer mem.grid ]


cellColor : Cell -> Color
cellColor cell =
    case cell of
        Empty ->
            white

        Red ->
            red

        Yellow ->
            yellow


gridCordToScreenCord : GridViewModel -> Grid.Cord -> ScreenCord
gridCordToScreenCord gvm ( x, y ) =
    ( gvm.left + toFloat x * gvm.cellSize, gvm.bottom + toFloat y * gvm.cellSize )


viewGridCellAt : Grid.Cord -> GridViewModel -> Maybe Shape
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


viewGrid : Computer -> Player -> Grid -> Shape
viewGrid { screen, mouse } player grid =
    let
        gvm =
            toGridViewModel screen grid

        frameOffset =
            gvm.cellSize

        frameWidth =
            gvm.width + frameOffset

        frameHeight =
            gvm.height + frameOffset

        nextMoveIndicator =
            circle (playerToCell player |> cellColor) gvm.cellRadius
                |> moveUp (frameHeight / 2 + gvm.cellRadius)
                |> moveRight moveIndicatorX

        moveIndicatorX =
            screenCordToGridCord ( mouse.x, mouse.y ) gvm
                |> Grid.clampCord grid
                |> gridCordToScreenCord gvm
                |> Tuple.first
    in
    group
        [ rectangle blue frameWidth frameHeight
        , List.filterMap (flip viewGridCellAt gvm) grid.cords |> group
        , nextMoveIndicator
        ]


type alias ScreenCord =
    ( Float, Float )


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
        maxCellWidth =
            (screen.width * 0.8) / toFloat (grid.width + 1)

        maxCellHeight =
            (screen.height * 0.8) / toFloat (grid.height + 1)

        cellSize =
            min maxCellWidth maxCellHeight |> round |> toFloat

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
