module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell, Coin(..), Grid)
import Playground exposing (..)
import PointFree exposing (flip)
import Set exposing (Set)



-- Game
-- Mem


type Mem
    = PlayerTurn Coin Grid
    | GameOver (Set Grid.Position) Coin Grid


initialMem : Mem
initialMem =
    PlayerTurn Grid.Red initialGrid


initialGrid : Grid
initialGrid =
    -- Grid.empty 6 5
    Grid.fromList 6
        5
        [ ( ( 1, 0 ), Red )
        , ( ( 2, 0 ), Red )
        , ( ( 3, 0 ), Red )
        ]


nextPlayerCoin : Coin -> Coin
nextPlayerCoin playerCoin =
    case playerCoin of
        Grid.Red ->
            Grid.Yellow

        Grid.Yellow ->
            Grid.Red



-- Update


update : Computer -> Mem -> Mem
update computer mem =
    case mem of
        PlayerTurn currentPlayerCoin grid ->
            case checkMouseClickOnGridColumn computer grid of
                Just column ->
                    case
                        Grid.insertCoinInColumn column currentPlayerCoin grid
                    of
                        Ok ( winningPositions, newGrid ) ->
                            if Set.isEmpty winningPositions then
                                PlayerTurn (nextPlayerCoin currentPlayerCoin) newGrid

                            else
                                GameOver winningPositions currentPlayerCoin newGrid

                        Err _ ->
                            mem

                Nothing ->
                    mem

        GameOver _ _ _ ->
            if computer.mouse.click then
                initialMem

            else
                mem



-- GridScreenModel


type alias ScreenPosition =
    ( Float, Float )


type alias GridScreenModel =
    { width : Float
    , height : Float
    , dx : Float
    , dy : Float
    , cellSize : Number
    , cellRadius : Number
    , grid_ : Grid
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
    , grid_ = grid
    }


checkMouseClickOnGridColumn : Computer -> Grid -> Maybe Int
checkMouseClickOnGridColumn { mouse, screen } grid =
    if mouse.click then
        screenPositionToGridPosition ( mouse.x, mouse.y ) (toGridScreenModel screen grid)
            |> Tuple.first
            |> Just

    else
        Nothing


screenPositionToGridPosition : ScreenPosition -> GridScreenModel -> Grid.Position
screenPositionToGridPosition ( x, y ) gsm =
    ( (x - gsm.dx) / gsm.cellSize |> round, (y - gsm.dy) / gsm.cellSize |> round )


gridPositionToScreenPosition : GridScreenModel -> Grid.Position -> ScreenPosition
gridPositionToScreenPosition gsm ( x, y ) =
    ( toFloat x * gsm.cellSize + gsm.dx, toFloat y * gsm.cellSize + gsm.dy )


snapMouseXToGrid : GridScreenModel -> Mouse -> Float
snapMouseXToGrid gsm mouse =
    screenPositionToGridPosition ( mouse.x, mouse.y ) gsm
        |> Grid.clampPosition gsm.grid_
        |> gridPositionToScreenPosition gsm
        |> Tuple.first


firstEmptyGridScreenPositionFromMouseX : Mouse -> GridScreenModel -> Maybe ScreenPosition
firstEmptyGridScreenPositionFromMouseX mouse gsm =
    screenPositionToGridPosition ( mouse.x, mouse.y ) gsm
        |> Tuple.first
        |> flip Grid.firstEmptyPositionInColumn gsm.grid_
        |> Maybe.map (gridPositionToScreenPosition gsm)



-- View


view : Computer -> Mem -> List Shape
view ({ screen } as computer) mem =
    [ rectangle lightBlue screen.width screen.height
    , case mem of
        PlayerTurn currentPlayerCoin grid ->
            viewPlayerTurn computer currentPlayerCoin grid

        GameOver winningPositions winningPlayerCoin grid ->
            viewGameOver computer winningPositions winningPlayerCoin grid
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


viewGridCell : GridScreenModel -> ( Grid.Position, Grid.Cell ) -> Shape
viewGridCell gsm ( cord, cell ) =
    moveShapeToGridPosition gsm (toCellShape gsm (cellToColor cell)) cord


viewGameOverGridCell : Time -> GridScreenModel -> Set Grid.Position -> ( Grid.Position, Grid.Cell ) -> Shape
viewGameOverGridCell time gsm gameOverPositions ( position, cell ) =
    let
        cellShape =
            toCellShape gsm (cellToColor cell)
                |> (if Set.member position gameOverPositions then
                        fade (wave 0.3 0.9 1.3 time + 0.1)

                    else
                        identity
                   )
    in
    moveShapeToGridPosition gsm cellShape position


moveShapeToGridPosition : GridScreenModel -> Shape -> Grid.Position -> Shape
moveShapeToGridPosition gsm shape =
    gridPositionToScreenPosition gsm
        >> (\( x, y ) -> shape |> move x y)


toCellShape : GridScreenModel -> Color -> Shape
toCellShape gsm color =
    group
        [ circle white gsm.cellRadius
        , circle color gsm.cellRadius
            |> scale 0.8
        ]


viewPlayerTurn : Computer -> Coin -> Grid -> Shape
viewPlayerTurn { screen, mouse, time } currentPlayerCoin grid =
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
            toCellShape gsm (coinToColor currentPlayerCoin)
                |> fade (wave 0.3 0.9 1.3 time + 0.1)

        nextMoveTopIndicator =
            nextMoveIndicatorShape
                |> moveUp (frameHeight / 2 + gsm.cellRadius)
                |> moveRight (snapMouseXToGrid gsm mouse)

        nextMoveCellIndicator =
            firstEmptyGridScreenPositionFromMouseX mouse gsm
                |> Maybe.map
                    (\( sx, sy ) -> nextMoveIndicatorShape |> move sx sy)
                |> Maybe.withDefault (group [])
    in
    group
        [ rectangle blue frameWidth frameHeight
        , List.map (viewGridCell gsm) (Grid.toCellList grid) |> group
        , nextMoveTopIndicator
        , nextMoveCellIndicator
        ]


viewGameOver : Computer -> Set Grid.Position -> Coin -> Grid -> Shape
viewGameOver { screen, mouse, time } winningPositions winningPlayerCoin grid =
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
    in
    group
        [ rectangle blue frameWidth frameHeight
        , List.map (viewGameOverGridCell time gsm winningPositions) (Grid.toCellList grid) |> group
        , words (coinToColor winningPlayerCoin) "Game Over, Click to Restart"
            |> moveDown (frameHeight / 2 + 20)
        ]


main =
    game view update initialMem
