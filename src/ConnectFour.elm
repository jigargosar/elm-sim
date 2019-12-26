module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell(..), Grid)
import Playground exposing (..)


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
view ({ screen } as computer) mem =
    [ rectangle lightBlue screen.width screen.height
    , viewGrid computer mem.currentPlayer mem.grid
    ]


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


viewGridCell : GridViewModel -> ( Grid.Cord, Grid.Cell ) -> Shape
viewGridCell gvm ( cord, cell ) =
    let
        ( x, y ) =
            gridCordToScreenCord gvm cord
    in
    cell |> cellToShape gvm |> move x y


cellToShape : GridViewModel -> Cell -> Shape
cellToShape gvm cell =
    circle (cellColor cell) gvm.cellRadius


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

        moveIndicatorScreenX =
            screenCordToGridCord ( mouse.x, mouse.y ) gvm
                |> Grid.clampCord grid
                |> gridCordToScreenCord gvm
                |> Tuple.first

        nextMoveTopIndicator =
            cellToShape gvm (playerToCell player)
                |> scale 0.8
                |> moveRight moveIndicatorScreenX
                |> moveUp (frameHeight / 2 + gvm.cellRadius)

        nextMoveCellIndicator =
            let
                ( x, _ ) =
                    screenCordToGridCord ( mouse.x, mouse.y ) gvm

                maybeScreenY =
                    Grid.getFirstNonEmptyCordWhereXEq x grid
                        |> Maybe.map (gridCordToScreenCord gvm >> Tuple.second)
            in
            case maybeScreenY of
                Just sy ->
                    cellToShape gvm (playerToCell player)
                        |> scale 0.8
                        |> moveRight moveIndicatorScreenX
                        |> moveUp sy

                Nothing ->
                    group []
    in
    group
        [ rectangle blue frameWidth frameHeight
        , List.map (viewGridCell gvm) (Grid.toList grid) |> group
        , nextMoveTopIndicator
        , nextMoveCellIndicator
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
        ( gw, gh ) =
            Grid.dimensions grid
                |> Tuple.mapBoth toFloat toFloat

        maxCellWidth =
            (screen.width * 0.8) / (gw + 1)

        maxCellHeight =
            (screen.height * 0.8) / (gh + 1)

        cellSize =
            min maxCellWidth maxCellHeight |> round |> toFloat

        width =
            (gw - 1) * cellSize

        height =
            (gh - 1) * cellSize
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
