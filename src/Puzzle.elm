module Puzzle exposing (main)

-- MAIN

import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (whenTrue)
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
    = RedCircle


init : Mem
init =
    { width = 10
    , height = 10
    , dict = Dict.empty
    , drag = NotDragging
    , pan = ( 0, 0 )
    , zoom = 1.5
    , prevMouse = Mouse 0 0 False False
    }
        |> insertTokenAt ( 0, 0 ) RedCircle
        |> insertTokenAt ( 0, 1 ) RedCircle
        |> insertTokenAt ( 0, 2 ) RedCircle
        |> insertTokenAt ( 3, 0 ) RedCircle
        |> insertTokenAt ( 3, 1 ) RedCircle
        |> insertTokenAt ( 3, 2 ) RedCircle


insertTokenAt : Pos -> Token -> Mem -> Mem
insertTokenAt pos token mem =
    if isPositionValid pos mem then
        { mem | dict = Dict.insert pos token mem.dict }

    else
        mem



-- UPDATE


update : Computer -> Mem -> Mem
update computer mem =
    let
        { screen, mouse } =
            computer

        worldMouse =
            let
                ( x, y ) =
                    cfg.screenToWorldPos ( mouse.x, mouse.y )
            in
            { mouse | x = x, y = y }

        prevWorldMouse =
            let
                prevMouse =
                    mem.prevMouse

                ( x, y ) =
                    cfg.screenToWorldPos ( prevMouse.x, prevMouse.y )
            in
            { prevMouse | x = x, y = y }

        cfg =
            toConfig screen mem

        worldComputer =
            { computer | mouse = worldMouse }
    in
    updateWorld worldComputer { mem | prevMouse = prevWorldMouse }
        |> (\m -> { m | prevMouse = computer.mouse })


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
                        cfg.worldToGridPos ( mouse.x, mouse.y )
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
                        cfg.worldToGridPos ( mouse.x, mouse.y )

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
            let
                ( px, py ) =
                    mem.pan

                ( dx, dy ) =
                    ( (mouse.x - prevMouse.x) * mem.zoom, (mouse.y - prevMouse.y) * mem.zoom )
            in
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
                        ( px + dx, py + dy )
            }


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
        RedCircle ->
            circle red (cfg.cellRadius * 0.75)


view : Computer -> Mem -> List Shape
view computer mem =
    let
        { screen, mouse } =
            computer

        worldMouse =
            let
                ( x, y ) =
                    cfg.screenToWorldPos ( mouse.x, mouse.y )
            in
            { mouse | x = x, y = y }

        cfg =
            toConfig screen mem

        worldComputer =
            { computer | mouse = worldMouse }
    in
    [ viewWorld worldComputer cfg mem
        |> group
        |> move mem.pan
        |> scale mem.zoom
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
                        |> move ( mouse.x, mouse.y )

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
                |> moveCell cfg pos
        )
        mem
        |> group
    , draggingShape
    ]



-- Main


main =
    game view update init



-- Transform


type Transform
    = Transform Float Float Float


noneT : Transform
noneT =
    Transform 0 0 1


translateT : Float -> Float -> Transform
translateT dx dy =
    Transform dx dy 1


scaleT : Float -> Transform
scaleT s =
    Transform 0 0 s


composeT : List Transform -> Transform
composeT list =
    let
        reducer (Transform dx0 dy0 s0) (Transform dx1 dy1 s1) =
            Transform (dx0 + dx1) (dy0 + dy1) (s0 + s1)
    in
    List.foldl reducer noneT list



-- Common


type alias Pos =
    ( Int, Int )


type alias Config =
    { cellSize : Float
    , cellRadius : Float
    , screenToWorldPos : ( Float, Float ) -> ( Float, Float )
    , worldToGridPos : ( Float, Float ) -> Pos
    , gridToWorldPos : Pos -> ( Float, Float )
    , width : Float
    , height : Float
    }


toConfig : Screen -> Mem -> Config
toConfig screen mem =
    let
        cellSize =
            min (screen.width / (toFloat mem.width + 1))
                (screen.height / (toFloat mem.height + 2))

        dx =
            (cellSize - (cellSize * toFloat mem.width)) / 2

        dy =
            (cellSize - (cellSize * toFloat mem.height)) / 2

        ( px, py ) =
            mem.pan

        gridToWorldPos ( gx, gy ) =
            ( (toFloat gx * cellSize) + dx, (toFloat gy * cellSize) + dy )

        screenToWorldPos ( x, y ) =
            ( (x - px) / mem.zoom, (y - py) / mem.zoom )

        worldToGridPos ( x, y ) =
            ( round ((x - dx) / cellSize), round ((y - dy) / cellSize) )
    in
    { cellSize = cellSize
    , cellRadius = cellSize / 2
    , gridToWorldPos = gridToWorldPos
    , worldToGridPos = worldToGridPos
    , screenToWorldPos = screenToWorldPos
    , width = cellSize * toFloat mem.width
    , height = cellSize * toFloat mem.height
    }


moveCell : Config -> Pos -> Shape -> Shape
moveCell cfg pos =
    move (cfg.gridToWorldPos pos)


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
