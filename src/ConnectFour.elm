module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell, Coin(..), Grid)
import Playground exposing (..)


type alias Mem =
    { grid : Grid
    , currentPlayerCoin : Grid.Coin
    }


initialMem : Mem
initialMem =
    { grid = initialGrid, currentPlayerCoin = Grid.Red }


initialGrid : Grid
initialGrid =
    Grid.empty 6 5


update : Computer -> Mem -> Mem
update computer mem =
    case mouseClickToGridColumn computer mem.grid of
        Just column ->
            case
                Grid.putCoinInColumn column mem.currentPlayerCoin mem.grid
            of
                Ok newGrid ->
                    { mem
                        | grid = newGrid
                        , currentPlayerCoin = nextPlayerCoin mem.currentPlayerCoin
                    }

                Err _ ->
                    mem

        Nothing ->
            mem


nextPlayerCoin : Coin -> Coin
nextPlayerCoin playerCoin =
    case playerCoin of
        Grid.Red ->
            Grid.Yellow

        Grid.Yellow ->
            Grid.Red


mouseClickToGridColumn : Computer -> Grid -> Maybe Int
mouseClickToGridColumn { mouse, screen } grid =
    if mouse.click then
        screenPositionToGridPosition ( mouse.x, mouse.y ) (toGridScreenModel screen grid)
            |> Tuple.first
            |> Just

    else
        Nothing


screenPositionToGridPosition : ScreenPosition -> GridScreenModel -> Grid.Position
screenPositionToGridPosition ( x, y ) gsm =
    ( (x - gsm.dx) / gsm.cellSize |> round, (y - gsm.dy) / gsm.cellSize |> round )


view : Computer -> Mem -> List Shape
view ({ screen } as computer) mem =
    [ rectangle lightBlue screen.width screen.height
    , viewGrid computer mem.currentPlayerCoin mem.grid
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


gridCordToScreenCord : GridScreenModel -> Grid.Position -> ScreenPosition
gridCordToScreenCord gsm ( x, y ) =
    ( toFloat x * gsm.cellSize + gsm.dx, toFloat y * gsm.cellSize + gsm.dy )


viewGridCell : GridScreenModel -> ( Grid.Position, Grid.Cell ) -> Shape
viewGridCell gsm ( cord, cell ) =
    let
        ( x, y ) =
            gridCordToScreenCord gsm cord
    in
    cellShapeWithColor gsm (cellToColor cell) |> move x y


cellShapeWithColor : GridScreenModel -> Color -> Shape
cellShapeWithColor gsm color =
    group
        [ circle white gsm.cellRadius
        , circle color gsm.cellRadius
            |> scale 0.8
        ]


viewGrid : Computer -> Coin -> Grid -> Shape
viewGrid { screen, mouse, time } currentPlayerCoin grid =
    let
        gsm =
            toGridScreenModel screen grid

        frameOffset =
            -- gsm.cellSize + (gsm.cellSize / 4)
            0

        frameWidth =
            gsm.width + frameOffset

        frameHeight =
            gsm.height + frameOffset

        nextMoveIndicatorShape =
            cellShapeWithColor gsm (coinToColor currentPlayerCoin)
                |> fade (wave 0.5 0.9 1.3 time + 0.1)
                |> moveRight
                    (screenPositionToGridPosition ( mouse.x, mouse.y ) gsm
                        |> Grid.clampPosition grid
                        |> gridCordToScreenCord gsm
                        |> Tuple.first
                    )

        nextMoveTopIndicator =
            nextMoveIndicatorShape
                |> moveUp (frameHeight / 2 + gsm.cellRadius)

        nextMoveCellIndicator =
            let
                ( column, _ ) =
                    screenPositionToGridPosition ( mouse.x, mouse.y ) gsm

                maybeScreenY =
                    Grid.firstEmptyPositionInColumn column grid
                        |> Maybe.map (gridCordToScreenCord gsm >> Tuple.second)
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
        , List.map (viewGridCell gsm) (Grid.toCellList grid) |> group
        , nextMoveTopIndicator
        , nextMoveCellIndicator
        ]


type alias ScreenPosition =
    ( Float, Float )


type alias GridScreenModel =
    { width : Float
    , height : Float
    , dx : Float
    , dy : Float
    , cellSize : Number
    , cellRadius : Number
    }


screenCellSize : Screen -> Grid -> Number
screenCellSize screen grid =
    let
        gridDimensions =
            Grid.dimensions grid

        maxCellWidth =
            (screen.width * 0.8) / (toFloat gridDimensions.width + 1)

        maxCellHeight =
            (screen.height * 0.8) / (toFloat gridDimensions.height + 1)
    in
    min maxCellWidth maxCellHeight


toGridScreenModel : Screen -> Grid -> GridScreenModel
toGridScreenModel screen grid =
    let
        gridDimensions =
            Grid.dimensions grid

        cellSize =
            screenCellSize screen grid

        width =
            toFloat gridDimensions.width * cellSize

        height =
            toFloat gridDimensions.height * cellSize
    in
    { width = width
    , height = height
    , dx = -width / 2 + cellSize / 2
    , dy = -height / 2 + cellSize / 2
    , cellSize = cellSize
    , cellRadius = cellSize / 2 - cellSize / 10
    }


main =
    game view update initialMem
