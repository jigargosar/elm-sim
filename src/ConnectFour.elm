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
update computer mem =
    case mouseClickToGridColumn computer mem.grid of
        Just column ->
            case
                Grid.putCoinInColumn column
                    (playerToCoin mem.currentPlayer)
                    mem.grid
            of
                Ok newGrid ->
                    { mem
                        | grid = newGrid
                        , currentPlayer = nextPlayer mem.currentPlayer
                    }

                Err _ ->
                    mem

        Nothing ->
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


mouseClickToGridColumn : Computer -> Grid -> Maybe Int
mouseClickToGridColumn { mouse, screen } grid =
    if mouse.click then
        Just <| mouseToGridColumn mouse (toGridScreenModel screen grid)

    else
        Nothing


mouseToGridColumn : Mouse -> GridScreenModel -> Int
mouseToGridColumn mouse =
    mouseToGridPosition mouse >> Tuple.first


mouseToGridRow : Mouse -> GridScreenModel -> Int
mouseToGridRow mouse =
    mouseToGridPosition mouse >> Tuple.second


mouseToGridPosition : Mouse -> GridScreenModel -> Grid.Position
mouseToGridPosition mouse =
    screenPositionToGridPosition ( mouse.x, mouse.y )


screenPositionToGridPosition : ScreenPosition -> GridScreenModel -> Grid.Position
screenPositionToGridPosition ( x, y ) gsm =
    ( (x - gsm.left) / gsm.cellSize |> round, (y - gsm.bottom) / gsm.cellSize |> round )


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


gridCordToScreenCord : GridScreenModel -> Grid.Position -> ScreenPosition
gridCordToScreenCord gsm ( x, y ) =
    ( gsm.left + toFloat x * gsm.cellSize, gsm.bottom + toFloat y * gsm.cellSize )


viewGridCell : GridScreenModel -> ( Grid.Position, Grid.Cell ) -> Shape
viewGridCell gsm ( cord, cell ) =
    let
        ( x, y ) =
            gridCordToScreenCord gsm cord
    in
    cell |> cellToShape gsm |> move x y


playerToCellShape : GridScreenModel -> Player -> Shape
playerToCellShape gsm player =
    cellShapeWithColor gsm (playerToCoin >> coinToColor <| player)


cellToShape : GridScreenModel -> Cell -> Shape
cellToShape gsm cell =
    cellShapeWithColor gsm (cellToColor cell)


cellShapeWithColor : GridScreenModel -> Color -> Shape
cellShapeWithColor gsm color =
    group
        [ circle white gsm.cellRadius
        , circle color gsm.cellRadius
            |> scale 0.8
        ]


viewGrid : Computer -> Player -> Grid -> Shape
viewGrid { screen, mouse, time } player grid =
    let
        gsm =
            toGridScreenModel screen grid

        frameOffset =
            gsm.cellSize + (gsm.cellSize / 4)

        frameWidth =
            gsm.width + frameOffset

        frameHeight =
            gsm.height + frameOffset

        nextMoveIndicatorShape =
            playerToCellShape gsm player
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
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
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
            (toFloat gridDimensions.width - 1) * cellSize

        height =
            (toFloat gridDimensions.height - 1) * cellSize
    in
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    , cellSize = cellSize
    , cellRadius = cellSize / 2 - cellSize / 10
    }


main =
    game view update initialMem
