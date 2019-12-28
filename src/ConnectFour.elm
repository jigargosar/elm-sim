module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell, Coin(..), Grid)
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


update : Computer -> Mem -> Mem
update { mouse, screen } mem =
    if mouse.click then
        let
            gvm =
                toGridViewModel screen mem.grid

            ( column, _ ) =
                screenPositionToGridPosition ( mouse.x, mouse.y ) gvm
        in
        case Grid.putCoinInColumn column (playerToCoin mem.currentPlayer) mem.grid of
            Ok newGrid ->
                { mem
                    | grid = newGrid
                    , currentPlayer = nextPlayer mem.currentPlayer
                }

            Err _ ->
                mem

    else
        mem


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        PlayerRed ->
            PlayerYellow

        PlayerYellow ->
            PlayerRed


playerToCoin : Player -> Coin
playerToCoin player =
    case player of
        PlayerRed ->
            Red

        PlayerYellow ->
            Yellow


screenPositionToGridPosition : ScreenPosition -> GridViewModel -> Grid.Position
screenPositionToGridPosition ( x, y ) gvm =
    ( (x - gvm.left) / gvm.cellSize |> round, (y - gvm.bottom) / gvm.cellSize |> round )


view : Computer -> Mem -> List Shape
view ({ screen } as computer) mem =
    [ rectangle lightBlue screen.width screen.height
    , viewGrid computer mem.currentPlayer mem.grid
    ]


cellToColor : Cell -> Color
cellToColor =
    Maybe.map coinToColor >> Maybe.withDefault white


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Yellow ->
            rgb 230 178 0



--  yellow


gridCordToScreenCord : GridViewModel -> Grid.Position -> ScreenPosition
gridCordToScreenCord gvm ( x, y ) =
    ( gvm.left + toFloat x * gvm.cellSize, gvm.bottom + toFloat y * gvm.cellSize )


viewGridCell : GridViewModel -> ( Grid.Position, Grid.Cell ) -> Shape
viewGridCell gvm ( cord, cell ) =
    let
        ( x, y ) =
            gridCordToScreenCord gvm cord
    in
    cell |> cellToShape gvm |> move x y


playerToCellShape : GridViewModel -> Player -> Shape
playerToCellShape gvm player =
    cellShapeWithColor gvm (playerToCoin >> coinToColor <| player)


cellToShape : GridViewModel -> Cell -> Shape
cellToShape gvm cell =
    cellShapeWithColor gvm (cellToColor cell)


cellShapeWithColor : GridViewModel -> Color -> Shape
cellShapeWithColor gvm color =
    group
        [ circle white gvm.cellRadius
        , circle color gvm.cellRadius
            |> scale 0.8
        ]


viewGrid : Computer -> Player -> Grid -> Shape
viewGrid { screen, mouse, time } player grid =
    let
        gvm =
            toGridViewModel screen grid

        frameOffset =
            gvm.cellSize + (gvm.cellSize / 4)

        frameWidth =
            gvm.width + frameOffset

        frameHeight =
            gvm.height + frameOffset

        nextMoveIndicatorShape =
            playerToCellShape gvm player
                |> fade (wave 0.5 0.9 1.3 time + 0.1)
                |> moveRight
                    (screenPositionToGridPosition ( mouse.x, mouse.y ) gvm
                        |> Grid.clampPosition grid
                        |> gridCordToScreenCord gvm
                        |> Tuple.first
                    )

        nextMoveTopIndicator =
            nextMoveIndicatorShape
                |> moveUp (frameHeight / 2 + gvm.cellRadius)

        nextMoveCellIndicator =
            let
                ( column, _ ) =
                    screenPositionToGridPosition ( mouse.x, mouse.y ) gvm

                maybeScreenY =
                    Grid.firstEmptyPositionInColumn column grid
                        |> Maybe.map (gridCordToScreenCord gvm >> Tuple.second)
            in
            case maybeScreenY of
                Just sy ->
                    nextMoveIndicatorShape
                        |> moveUp sy

                Nothing ->
                    group []
    in
    group
        [ rectangle blue frameWidth frameHeight
        , List.map (viewGridCell gvm) (Grid.toCellList grid) |> group
        , nextMoveTopIndicator
        , nextMoveCellIndicator
        ]


type alias ScreenPosition =
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
    }


toGridViewModel : Screen -> Grid -> GridViewModel
toGridViewModel screen grid =
    let
        ( gw, gh ) =
            Grid.dimensionsToTuple grid
                |> Tuple.mapBoth toFloat toFloat

        maxCellWidth =
            (screen.width * 0.8) / (gw + 1)

        maxCellHeight =
            (screen.height * 0.8) / (gh + 1)

        cellSize =
            min maxCellWidth maxCellHeight

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
    , cellRadius = cellSize / 2 - cellSize / 10 |> round |> toFloat
    }


main =
    game view update initialMem
