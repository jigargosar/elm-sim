module ConnectFourV3Kata1.Main exposing (main)

import ConnectFourV3Kata1.Board as Board exposing (Board)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (flip, whenTrue)
import Set exposing (Set)



-- MODELS


type alias Pos =
    ( Int, Int )



-- DIMENSIONS


type alias IntDim =
    { width : Int, height : Int }


type alias FloatDim =
    { width : Float, height : Float }


toFloatDim : { a | width : Int, height : Int } -> FloatDim
toFloatDim { width, height } =
    { width = toFloat width, height = toFloat height }


scaleDim : number -> { a | width : number, height : number } -> { width : number, height : number }
scaleDim sc { width, height } =
    { width = width * sc, height = height * sc }


fDivDim : { a | width : Float, height : Float } -> { b | width : Float, height : Float } -> FloatDim
fDivDim a b =
    FloatDim (a.width / b.width) (a.height / b.height)


minDim : { a | width : comparable, height : comparable } -> comparable
minDim { width, height } =
    min width height



-- MEM


type alias Mem =
    { dim : IntDim
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
    , selectedColumn = Board.centerColumn board
    }


resetBoard : Mem -> Mem
resetBoard mem =
    { mem
        | board = Board.reset mem.board
        , selectedColumn = Board.centerColumn mem.board
    }



-- UPDATE


type alias BoardState =
    Board.State


toBoardState : Board -> BoardState
toBoardState =
    Board.state


update : Computer -> Mem -> Mem
update computer mem =
    let
        cfg =
            toConfig computer mem
    in
    case toBoardState mem.board of
        Board.Turn _ ->
            if computer.mouse.click then
                let
                    column =
                        ((computer.mouse.x + cfg.dx) / cfg.cellSize)
                            |> round
                            |> flip Board.clampColumn mem.board
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



-- VIEW MODEL


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
        gDim : FloatDim
        gDim =
            Board.dimensions mem.board |> toFloatDim

        sDim : FloatDim
        sDim =
            computer.screen |> scaleDim 0.7

        cellSize =
            fDivDim sDim gDim |> minDim

        ( widthPx, heightPx ) =
            ( boardScreenDim.width, boardScreenDim.height )

        boardScreenDim =
            scaleDim cellSize gDim
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
    case Board.insertPositionFromColumn mem.selectedColumn mem.board of
        Just pos ->
            Dict.insert pos (Cell True player)

        Nothing ->
            identity



-- View


view : Computer -> Mem -> List Shape
view computer mem =
    let
        c =
            toConfig computer mem

        cellDict =
            Board.toDict mem.board
                |> Dict.map (\_ -> Cell False)
                |> (case state of
                        Board.Turn player ->
                            insertIndicatorCell player mem

                        Board.Victory _ wps ->
                            highlightCells wps

                        Board.Draw ->
                            identity
                   )

        state =
            toBoardState mem.board

        topIndicatorShape =
            case state of
                Board.Turn player ->
                    indicatorShape computer.time c.cellSize player
                        |> translateTopIndicator c mem

                _ ->
                    noShape

        cellAt pos =
            Dict.get pos cellDict
    in
    [ rectangle gray computer.screen.width computer.screen.height
    , rectangle black c.width c.height
    , Board.allPositions mem.board
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
