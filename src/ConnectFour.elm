module ConnectFour exposing (main)

import ConnectFour.Grid as Grid exposing (Cell, Coin(..), Grid)
import Playground exposing (..)
import PointFree exposing (flip, mapEach)
import Set exposing (Set)



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
    , bottom : Number
    , top : Number
    , dx : Float
    , dy : Float
    , cellSize : Number
    , cellRadius : Number
    , grid_ : Grid
    }


screenCellSize : Screen -> Grid -> Number
screenCellSize screen grid =
    let
        ( columns, rows ) =
            ( Grid.width grid, Grid.height grid )
                |> mapEach (toFloat >> (+) 1)

        ( screenWidth, screenHeight ) =
            ( screen.width, screen.height )
                |> mapEach ((*) 0.8)
    in
    min (screenWidth / columns) (screenHeight / rows)


toGridScreenModel : Screen -> Grid -> GridScreenModel
toGridScreenModel screen grid =
    let
        cellSize : Number
        cellSize =
            screenCellSize screen grid

        ( width, height ) =
            ( Grid.width grid, Grid.height grid )
                |> mapEach (toFloat >> (*) cellSize)
    in
    { width = width
    , height = height
    , bottom = -height / 2
    , top = height / 2
    , dx = -width / 2 + cellSize / 2
    , dy = -height / 2 + cellSize / 2
    , cellSize = cellSize
    , cellRadius = cellSize / 2 - cellSize / 10
    , grid_ = grid
    }


checkMouseClickOnGridColumn : Computer -> Grid -> Maybe Int
checkMouseClickOnGridColumn { mouse, screen } grid =
    if mouse.click then
        toGridPosition ( mouse.x, mouse.y ) (toGridScreenModel screen grid)
            |> Tuple.first
            |> Just

    else
        Nothing


toGridPosition : ScreenPosition -> GridScreenModel -> Grid.Position
toGridPosition ( x, y ) gsm =
    ( (x - gsm.dx) / gsm.cellSize |> round, (y - gsm.dy) / gsm.cellSize |> round )


toScreenPosition : GridScreenModel -> Grid.Position -> ScreenPosition
toScreenPosition gsm ( x, y ) =
    ( toFloat x * gsm.cellSize + gsm.dx, toFloat y * gsm.cellSize + gsm.dy )


snapMouseXToGrid : GridScreenModel -> Mouse -> Float
snapMouseXToGrid gsm mouse =
    toGridPosition ( mouse.x, mouse.y ) gsm
        |> Grid.clampPosition gsm.grid_
        |> toScreenPosition gsm
        |> Tuple.first


firstEmptyGridPositionFromMouseX : Mouse -> GridScreenModel -> Maybe Grid.Position
firstEmptyGridPositionFromMouseX mouse gsm =
    toGridPosition ( mouse.x, mouse.y ) gsm
        |> Tuple.first
        |> flip Grid.firstEmptyPositionInColumn gsm.grid_



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


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Red ->
            red

        Yellow ->
            rgb 230 178 0


viewGridCoin : GridScreenModel -> ( Grid.Position, Grid.Coin ) -> Shape
viewGridCoin gsm ( position, coin ) =
    coinToShape gsm coin
        |> placeOnScreen gsm position


viewEmptyGridCell : GridScreenModel -> Grid.Position -> Shape
viewEmptyGridCell gsm position =
    emptyCellShape gsm
        |> placeOnScreen gsm position


viewGameOverGridCoin : Time -> GridScreenModel -> Set Grid.Position -> ( Grid.Position, Grid.Coin ) -> Shape
viewGameOverGridCoin time gsm gameOverPositions ( gridPosition, coin ) =
    viewGridCoin gsm ( gridPosition, coin )
        |> (if Set.member gridPosition gameOverPositions then
                fade (wave 0.3 0.9 1.3 time + 0.1)

            else
                identity
           )


move : ( Number, Number ) -> Shape -> Shape
move ( x, y ) =
    Playground.move x y


placeOnScreen : GridScreenModel -> Grid.Position -> Shape -> Shape
placeOnScreen gsm gridPosition =
    toScreenPosition gsm gridPosition
        |> move


coinToShape : GridScreenModel -> Coin -> Shape
coinToShape gsm coin =
    circle (coinToColor coin) gsm.cellRadius
        |> scale 0.8


emptyCellShape : GridScreenModel -> Shape
emptyCellShape gsm =
    circle white gsm.cellRadius


viewPlayerTurn : Computer -> Coin -> Grid -> Shape
viewPlayerTurn { screen, mouse, time } currentPlayerCoin grid =
    let
        gsm =
            toGridScreenModel screen grid

        nextMoveIndicatorShape =
            group
                [ emptyCellShape gsm
                , coinToShape gsm currentPlayerCoin
                    |> fade (wave 0.3 0.9 1.3 time + 0.1)
                ]

        nextMoveTopIndicator =
            nextMoveIndicatorShape
                |> moveY gsm.top
                |> moveUp (gsm.cellSize / 2)
                |> moveRight (snapMouseXToGrid gsm mouse)

        nextMoveCellIndicator =
            firstEmptyGridPositionFromMouseX mouse gsm
                |> Maybe.map
                    (\gridPosition ->
                        nextMoveIndicatorShape
                            |> placeOnScreen gsm gridPosition
                    )
                |> Maybe.withDefault (group [])
    in
    group
        [ rectangle blue gsm.width gsm.height
        , List.map (viewEmptyGridCell gsm) (Grid.allPositions grid) |> group
        , List.map (viewGridCoin gsm) (Grid.toList grid) |> group
        , nextMoveTopIndicator
        , nextMoveCellIndicator
        ]


viewGameOver : Computer -> Set Grid.Position -> Coin -> Grid -> Shape
viewGameOver { screen, mouse, time } winningPositions winningPlayerCoin grid =
    let
        gsm =
            toGridScreenModel screen grid
    in
    group
        [ rectangle blue gsm.width gsm.height
        , List.map (viewEmptyGridCell gsm) (Grid.allPositions grid) |> group
        , List.map (viewGameOverGridCoin time gsm winningPositions) (Grid.toList grid) |> group
        , words (coinToColor winningPlayerCoin) "Game Over, Click to Restart"
            |> moveY gsm.bottom
            |> moveDown 20
        ]


main =
    game view update initialMem
