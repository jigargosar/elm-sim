module ConnectFourV3Kata1.Main exposing (main)

import ConnectFourV3Kata1.Board as Board exposing (Board)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (flip, whenTrue)
import Set exposing (Set)



-- MODELS


type alias Pos =
    ( Int, Int )


type alias ScreenPos =
    ( Number, Number )



-- DIMENSIONS


type alias Dim =
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


subDim :
    { a | width : number, height : number }
    -> { b | width : number, height : number }
    -> { width : number, height : number }
subDim a b =
    { width = a.width - b.width, height = a.height - b.height }


mapDimAsTuple : (( a, b ) -> ( c, d )) -> { e | width : a, height : b } -> { width : c, height : d }
mapDimAsTuple func { width, height } =
    func ( width, height ) |> dimFromTuple


dimFromTuple : ( a, b ) -> { width : a, height : b }
dimFromTuple ( width, height ) =
    { width = width, height = height }



-- MEM


type alias Mem =
    { board : Board
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
    { board =
        [ 0, 1, 0, 1, 0, 1, 0, 1 ]
            |> List.foldl Board.insertInColumn board
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
                        toBoardX cfg computer.mouse.x

                    -- |> flip Board.clampColumn mem.board
                in
                if column == mem.selectedColumn then
                    { mem | board = Board.insertInColumn column mem.board }

                else
                    { mem | selectedColumn = Board.clampColumn column mem.board }

            else
                mem

        _ ->
            if computer.mouse.click then
                resetBoard mem

            else
                mem



-- VIEW MODEL


type alias Config =
    { dim : FloatDim
    , cellDim : FloatDim
    , cellT : Transform
    }


toBoardX : Config -> Float -> Int
toBoardX cfg x =
    applyInverse cfg.cellT ( x, 1 ) |> Tuple.first |> round


toScreenPos : Config -> Pos -> ScreenPos
toScreenPos { cellT } ( gx, gy ) =
    applyTransform cellT ( toFloat gx, toFloat gy )


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

        dim =
            scaleDim cellSize gDim

        cellDim =
            FloatDim cellSize cellSize

        translateDim =
            subDim dim cellDim |> scaleDim -0.5
    in
    { dim = dim
    , cellDim = cellDim
    , cellT =
        Transform
            [ Scale cellDim.width cellDim.height
            , Translate translateDim.width translateDim.height
            ]
    }


type Transform
    = Translate Float Float
    | Scale Float Float
    | Transform (List Transform)


applyTransform : Transform -> ( Float, Float ) -> ( Float, Float )
applyTransform t ( x, y ) =
    case t of
        Translate dx dy ->
            ( x + dx, y + dy )

        Scale sx sy ->
            ( x * sx, y * sy )

        Transform list ->
            List.foldl applyTransform ( x, y ) list


applyInverse : Transform -> ( Float, Float ) -> ( Float, Float )
applyInverse t ( x, y ) =
    case t of
        Translate dx dy ->
            ( x - dx, y - dy )

        Scale sx sy ->
            ( x / sx, y / sy )

        Transform list ->
            List.foldr applyInverse ( x, y ) list


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
        cfg =
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

        dim =
            Board.dimensions mem.board

        topIndicatorShape =
            case state of
                Board.Turn player ->
                    indicatorShape computer.time cfg.cellDim player
                        |> translateCell cfg ( mem.selectedColumn, dim.height )

                _ ->
                    noShape

        cellAt pos =
            Dict.get pos cellDict
    in
    [ rectangle2 gray computer.screen
    , rectangle2 black cfg.dim
    , Board.allPositions mem.board
        |> List.indexedMap
            (\idx pos ->
                [ toCellShape computer.time cfg.cellDim (cellAt pos)

                --, words black (Debug.toString pos)
                , words black (Debug.toString idx)
                ]
                    |> group
                    |> translateCell cfg pos
            )
        |> group
    , topIndicatorShape
    ]


rectangle2 : Color -> { a | width : Number, height : Number } -> Shape
rectangle2 color { width, height } =
    rectangle color width height


toCellShape : Time -> FloatDim -> Maybe Cell -> Shape
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


indicatorShape : Time -> FloatDim -> Board.Player -> Shape
indicatorShape time cellSize player =
    cellPlayerShape cellSize player
        |> blink time


translateCell : Config -> Pos -> Shape -> Shape
translateCell cfg pos =
    move (toScreenPos cfg pos)


move : ScreenPos -> Shape -> Shape
move ( x, y ) =
    Playground.move x y


oval2 c { width, height } =
    Playground.oval c width height


cellBgShape : FloatDim -> Shape
cellBgShape cellSize =
    oval2 white (cellSize |> scaleDim 0.9)


cellPlayerShape : FloatDim -> Board.Player -> Shape
cellPlayerShape cellSize player =
    oval2
        (case player of
            Board.P1 ->
                blue

            Board.P2 ->
                red
        )
        (cellSize |> scaleDim 0.75)


blink : Time -> Shape -> Shape
blink =
    zigzag 0.5 1 1 >> fade



-- MAIN


main =
    game view update init
