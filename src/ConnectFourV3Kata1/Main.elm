module ConnectFourV3Kata1.Main exposing (main)

import ConnectFourV3Kata1.Board as Board exposing (Board)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (flip, inc, whenTrue)
import Set exposing (Set)



-- MODELS


type alias Pos =
    ( Int, Int )



-- GRID DIMENSION


type alias GDim =
    { width : Int, height : Int }


centerColumn : GDim -> Int
centerColumn { width } =
    width // 2


gridPositions : GDim -> List Pos
gridPositions { width, height } =
    List.range 0 (height - 1)
        |> List.concatMap
            (\y -> List.range 0 (width - 1) |> List.map (\x -> ( x, y )))


validatePos : Pos -> GDim -> Maybe Pos
validatePos pos { width, height } =
    let
        ( gx, gy ) =
            pos

        isOk =
            (gx < 0 || gy < 0 || gx >= width || gy >= height)
                |> not
    in
    if isOk then
        Just pos

    else
        Nothing



-- MEM


type alias Mem =
    { dim : GDim
    , board : Board
    , selectedColumn : Int
    }


init : Mem
init =
    let
        dim =
            { width = 7, height = 6 }

        board =
            Board.init dim
    in
    { dim = dim
    , board = [ 0, 1, 0, 1, 0, 1, 0, 1 ] |> List.foldl Board.insertInColumn board
    , selectedColumn = centerColumn dim
    }


resetBoard : Mem -> Mem
resetBoard mem =
    let
        dim =
            mem.dim
    in
    { mem | board = Board.init dim, selectedColumn = centerColumn dim }


insertPosition : Mem -> Maybe Pos
insertPosition mem =
    let
        colLength =
            Dict.foldl (\( x, _ ) _ -> whenTrue (x == mem.selectedColumn) inc) 0 (Board.toDict mem.board)
    in
    validatePos ( mem.selectedColumn, colLength ) mem.dim



-- UPDATE


type BoardState
    = Turn Board.Player
    | Draw
    | Victory Board.Player (Set Pos)


toBoardState : Board -> BoardState
toBoardState =
    Board.transformState
        { playerWon = Victory
        , playerTurn = Turn
        , gameDraw = always Draw
        }


update : Computer -> Mem -> Mem
update computer mem =
    let
        cfg =
            toConfig computer mem
    in
    case toBoardState mem.board of
        Turn _ ->
            if computer.mouse.click then
                let
                    column =
                        ((computer.mouse.x + cfg.dx) / cfg.cellSize)
                            |> round
                            |> clamp 0 (mem.dim.width - 1)
                in
                if column == mem.selectedColumn then
                    { mem | board = Board.insertInColumn column mem.board }

                else
                    { mem | selectedColumn = column }

            else
                mem

        _ ->
            if computer.mouse.click then
                resetBoard mem

            else
                mem



-- VIEW


type alias Config =
    { cellSize : Float
    , width : Float
    , height : Float
    , dx : Float
    , dy : Float
    }


toConfig : Computer -> Mem -> Config
toConfig computer mem =
    let
        cellSize =
            min (computer.screen.width * 0.7 / toFloat mem.dim.width)
                (computer.screen.height * 0.7 / toFloat mem.dim.height)

        ( widthPx, heightPx ) =
            ( toFloat mem.dim.width * cellSize, toFloat mem.dim.height * cellSize )
    in
    { cellSize = cellSize
    , width = widthPx
    , height = heightPx
    , dx = (widthPx - cellSize) / 2
    , dy = (heightPx - cellSize) / 2
    }


type Cell
    = Cell Bool Board.Player


highlightCell : Cell -> Cell
highlightCell (Cell _ p) =
    Cell True p


highlightCells : Set Pos -> Dict Pos Cell -> Dict Pos Cell
highlightCells =
    Set.foldl (\pos -> Dict.update pos (Maybe.map highlightCell))
        |> flip


insertIndicatorCell : Board.Player -> Mem -> Dict Pos Cell -> Dict Pos Cell
insertIndicatorCell player mem =
    case insertPosition mem of
        Just pos ->
            Dict.insert pos (Cell True player)

        Nothing ->
            identity


view : Computer -> Mem -> List Shape
view computer mem =
    let
        c =
            toConfig computer mem

        cellDict =
            Board.toDict mem.board
                |> Dict.map (\_ -> Cell False)
                |> (case state of
                        Turn player ->
                            insertIndicatorCell player mem

                        Victory _ wps ->
                            highlightCells wps

                        Draw ->
                            identity
                   )

        state =
            toBoardState mem.board

        topIndicatorShape =
            case state of
                Turn player ->
                    indicatorShape computer.time c.cellSize player
                        |> translateTopIndicator c mem

                _ ->
                    noShape

        cellAt pos =
            Dict.get pos cellDict
    in
    [ rectangle gray computer.screen.width computer.screen.height
    , rectangle black c.width c.height
    , gridPositions mem.dim
        |> List.indexedMap
            (\idx pos ->
                [ toCellShape computer.time c.cellSize (cellAt pos)

                --, words black (Debug.toString pos)
                , words black (Debug.toString idx)
                ]
                    |> group
                    |> translateCell c pos
            )
        |> group
    , topIndicatorShape
    ]


toCellShape : Time -> Float -> Maybe Cell -> Shape
toCellShape time cellSize maybeCell =
    case maybeCell of
        Just (Cell bool player) ->
            [ cellBgShape cellSize
            , cellPlayerShape cellSize player
                |> whenTrue bool (blink time)
            ]
                |> group

        Nothing ->
            cellBgShape cellSize


noShape : Shape
noShape =
    group []


indicatorShape : Time -> Float -> Board.Player -> Shape
indicatorShape time cellSize player =
    cellPlayerShape cellSize player
        |> blink time


translateTopIndicator : Config -> Mem -> Shape -> Shape
translateTopIndicator c mem =
    translateCell c ( mem.selectedColumn, mem.dim.height )


translateCell : { a | cellSize : Float, dx : Float, dy : Float } -> Pos -> Shape -> Shape
translateCell { cellSize, dx, dy } ( gx, gy ) =
    let
        ( x, y ) =
            ( toFloat gx * cellSize - dx, toFloat gy * cellSize - dy )
    in
    move x y


cellBgShape : Float -> Shape
cellBgShape cellSize =
    circle white (cellSize * 0.5 * 0.9)


cellPlayerShape : Float -> Board.Player -> Shape
cellPlayerShape cellSize player =
    circle
        (case player of
            Board.P1 ->
                blue

            Board.P2 ->
                red
        )
        (cellSize * 0.5 * 0.75)


blink : Time -> Shape -> Shape
blink =
    zigzag 0.5 1 1 >> fade



-- MAIN


main =
    game view update init
