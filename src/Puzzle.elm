module Puzzle exposing (main)

-- MAIN

import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (mapEach, mulBy, whenTrue)
import Set



-- MODEL


type alias Mem =
    { width : Int
    , height : Int
    , dict : Dict Pos Token
    , drag : Drag
    , pan : ( Float, Float )
    , zoom : Float
    , prevMouse : Mouse
    }


type Drag
    = NotDragging
    | Dragging Pos Token
    | Panning ( Float, Float )


type Token
    = Source
    | Mirror Int Int


init : Mem
init =
    { width = 5
    , height = 5
    , dict = Dict.empty
    , drag = NotDragging
    , pan = ( 0, 0 )
    , zoom = 1
    , prevMouse = Mouse 0 0 False False
    }
        |> insertTokenAt ( 1, 3 ) Source
        |> insertTokenAt ( 3, 3 ) (Mirror 1 1)


insertTokenAt : Pos -> Token -> Mem -> Mem
insertTokenAt pos token mem =
    if isPositionValid pos mem then
        { mem | dict = Dict.insert pos token mem.dict }

    else
        mem



-- UPDATE


update : Computer -> Mem -> Mem
update computer =
    updateWorld computer
        >> (\mem -> { mem | prevMouse = computer.mouse })


updateWorld : Computer -> Mem -> Mem
updateWorld computer mem =
    let
        { screen, mouse, keyboard } =
            computer

        prevMouse =
            mem.prevMouse

        cfg =
            toConfig screen mem
    in
    case mem.drag of
        NotDragging ->
            if mouse.down && keyboard.shift then
                { mem | drag = Panning mem.pan }

            else if mouse.down && not prevMouse.down then
                let
                    pos =
                        cfg.screenToGridCell ( mouse.x, mouse.y )
                in
                case Dict.get pos mem.dict of
                    Just token ->
                        { mem | drag = Dragging pos token }

                    Nothing ->
                        { mem | drag = Panning mem.pan }

            else if keyboard.space then
                { mem | pan = ( 0, 0 ), zoom = 1 }

            else if plusDown keyboard || Set.member "]" keyboard.keys then
                { mem | zoom = clamp 0.1 3 (mem.zoom + (0.05 * mem.zoom)) }

            else if minusDown keyboard || Set.member "[" keyboard.keys then
                { mem | zoom = clamp 0.1 3 (mem.zoom - (0.05 * mem.zoom)) }

            else
                mem

        Dragging pos token ->
            if not mouse.down then
                let
                    dropPos =
                        cfg.screenToGridCell ( mouse.x, mouse.y )

                    newDict =
                        if isPositionValid dropPos mem then
                            Dict.remove pos mem.dict
                                |> Dict.insert dropPos token
                                |> (\dict ->
                                        Dict.get dropPos mem.dict
                                            |> Maybe.map (\tk -> Dict.insert pos tk dict)
                                            |> Maybe.withDefault dict
                                   )

                        else
                            mem.dict
                in
                { mem
                    | drag = NotDragging
                    , dict = newDict
                }

            else
                mem

        Panning orignalPan ->
            { mem
                | drag =
                    if not mouse.down || escDown keyboard then
                        NotDragging

                    else
                        mem.drag
                , pan =
                    if escDown keyboard then
                        orignalPan

                    else
                        addVec mem.pan (vecFromTo prevMouse mouse)
            }


vecFromTo : { b | x : number, y : number } -> { a | x : number, y : number } -> ( number, number )
vecFromTo r1 r2 =
    ( r2.x - r1.x, r2.y - r1.y )


addVec : ( number, number ) -> ( number, number ) -> ( number, number )
addVec ( a, b ) ( c, d ) =
    ( a + c, b + d )


escDown { keys } =
    Set.member "Escape" keys


plusDown { keys } =
    Set.member "+" keys


minusDown { keys } =
    Set.member "-" keys



-- VIEW


tokenShape : { a | cellRadius : Float } -> Token -> Shape
tokenShape cfg token =
    case token of
        Source ->
            rectangle orange cfg.cellRadius cfg.cellRadius

        Mirror _ _ ->
            oval lightGreen (cfg.cellRadius / 1.8) (cfg.cellRadius * 1.5)
                |> moveLeft ((cfg.cellRadius / 1.8) / 2)


view : Computer -> Mem -> List Shape
view computer mem =
    let
        cfg =
            toConfig computer.screen mem
    in
    [ viewWorld computer cfg mem
        |> group
        |> scale mem.zoom
        |> move mem.pan
    ]


viewWorld : Computer -> Config -> Mem -> List Shape
viewWorld { mouse } cfg mem =
    let
        draggingPos : Maybe Pos
        draggingPos =
            case mem.drag of
                Dragging pos _ ->
                    Just pos

                _ ->
                    Nothing

        draggingToken : Maybe Token
        draggingToken =
            case mem.drag of
                Dragging _ token ->
                    Just token

                _ ->
                    Nothing

        dimIfDragging pos =
            whenTrue (Just pos == draggingPos)
                (fade 0.5)

        draggingShape =
            case draggingToken of
                Just token ->
                    tokenShape cfg token
                        |> move (cfg.screenToWorld ( mouse.x, mouse.y ))

                Nothing ->
                    noShape

        tokenShapeAt pos =
            case Dict.get pos mem.dict of
                Just token ->
                    tokenShape cfg token
                        |> dimIfDragging pos

                Nothing ->
                    noShape
    in
    [ rectangle black cfg.width cfg.height
    , mapAllPos
        (\pos ->
            [ circle white (cfg.cellRadius * 0.9)
            , tokenShapeAt pos
            ]
                |> group
                |> move (cfg.gridCellToWorld pos)
        )
        mem
        |> group
    , draggingShape
    ]



-- Main


main =
    game view update init



-- Common


type alias Pos =
    ( Int, Int )


type alias Config =
    { cellSize : Float
    , cellRadius : Float
    , screenToWorld : ( Float, Float ) -> ( Float, Float )
    , worldToScreen : ( Float, Float ) -> ( Float, Float )
    , worldToGridCell : ( Float, Float ) -> Pos
    , screenToGridCell : ( Float, Float ) -> Pos
    , gridCellToWorld : Pos -> ( Float, Float )
    , gridCellToScreen : Pos -> ( Float, Float )
    , width : Float
    , height : Float
    }


toConfig : Screen -> Mem -> Config
toConfig screen mem =
    let
        cellSize =
            min (screen.width / (toFloat mem.width + 1))
                (screen.height / (toFloat mem.height + 2))

        ( dx, dy ) =
            ( (cellSize - (cellSize * toFloat mem.width)) / 2
            , (cellSize - (cellSize * toFloat mem.height)) / 2
            )

        gridCellToWorld =
            mapEach toFloat
                >> (\( gx, gy ) -> ( (gx * cellSize) + dx, (gy * cellSize) + dy ))

        worldToGridCell ( x, y ) =
            ( (x - dx) / cellSize, (y - dy) / cellSize ) |> mapEach round

        ( px, py ) =
            mem.pan

        screenToWorld ( x, y ) =
            ( (x - px) / mem.zoom, (y - py) / mem.zoom )

        worldToScreen ( x, y ) =
            ( (x * mem.zoom) + px, (y * mem.zoom) + py )

        screenToGridCell =
            screenToWorld >> worldToGridCell

        gridCellToScreen =
            gridCellToWorld >> worldToScreen
    in
    { cellSize = cellSize
    , cellRadius = cellSize / 2
    , gridCellToWorld = gridCellToWorld
    , worldToGridCell = worldToGridCell
    , screenToWorld = screenToWorld
    , worldToScreen = worldToScreen
    , screenToGridCell = screenToGridCell
    , gridCellToScreen = gridCellToScreen
    , width = cellSize * toFloat mem.width
    , height = cellSize * toFloat mem.height
    }


moveCell : Config -> Pos -> Shape -> Shape
moveCell cfg pos =
    move (cfg.gridCellToWorld pos)


move : ( Number, Number ) -> Shape -> Shape
move ( x, y ) =
    Playground.move x y


mapAllPos : (Pos -> b) -> { a | width : Int, height : Int } -> List b
mapAllPos func mem =
    List.map func (allPos mem)


allPos : { a | width : Int, height : Int } -> List Pos
allPos { width, height } =
    List.range 0 (height - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (width - 1)
                    |> List.map (\x -> ( x, y ))
            )


isValidIdx : Int -> Int -> Bool
isValidIdx idx len =
    idx >= 0 && idx < len


isPositionValid : Pos -> { a | width : Int, height : Int } -> Bool
isPositionValid ( x, y ) mem =
    isValidIdx x mem.width && isValidIdx y mem.height


noShape =
    group []
