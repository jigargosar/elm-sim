module Puzzle exposing (main)

-- MAIN

import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (whenTrue)



-- MODEL


type alias Mem =
    { width : Int
    , height : Int
    , dict : Dict Pos Token
    , drag : Drag
    , pan : ( Float, Float )
    }


type Drag
    = NotDragging
    | Dragging Pos Token
    | Panning Float Float


type Token
    = RedCircle


init : Mem
init =
    { width = 10
    , height = 10
    , dict = Dict.empty
    , drag = NotDragging
    , pan = ( 0, 0 )
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
        { screen, mouse, keyboard } =
            computer

        cfg =
            toConfig screen mem
    in
    case mem.drag of
        NotDragging ->
            if mouse.down && keyboard.shift then
                { mem | drag = Panning mouse.x mouse.y }

            else if mouse.down then
                let
                    pos =
                        cfg.toGrid ( mouse.x, mouse.y )
                in
                case Dict.get pos mem.dict of
                    Just token ->
                        { mem | drag = Dragging pos token }

                    Nothing ->
                        mem

            else
                mem

        Dragging pos token ->
            if not mouse.down then
                let
                    dropPos =
                        cfg.toGrid ( mouse.x, mouse.y )

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

        Panning sx sy ->
            let
                ( px, py ) =
                    mem.pan

                ( dx, dy ) =
                    ( mouse.x - sx, mouse.y - sy )
            in
            { mem
                | drag =
                    if not mouse.down || not keyboard.shift then
                        NotDragging

                    else
                        mem.drag
                , pan = ( px + dx, py + dy )
            }



-- VIEW


tokenShape : { a | cellRadius : Float } -> Token -> Shape
tokenShape cfg token =
    case token of
        RedCircle ->
            circle red (cfg.cellRadius * 0.75)


view : Computer -> Mem -> List Shape
view { screen, mouse } mem =
    let
        cfg =
            toConfig screen mem

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



-- Common


type alias Pos =
    ( Int, Int )


type alias Config =
    { cellSize : Float
    , cellRadius : Float
    , toScreen : Pos -> ( Float, Float )
    , toGrid : ( Float, Float ) -> Pos
    , width : Float
    , height : Float
    }


toConfig : { a | width : Float, height : Float } -> { b | width : Int, height : Int } -> Config
toConfig screen mem =
    let
        cellSize =
            min (screen.width / (toFloat mem.width + 1))
                (screen.height / (toFloat mem.height + 2))

        dx =
            (cellSize - (cellSize * toFloat mem.width)) / 2

        dy =
            (cellSize - (cellSize * toFloat mem.height)) / 2

        toScreen ( gx, gy ) =
            ( (toFloat gx * cellSize) + dx, (toFloat gy * cellSize) + dy )

        toGrid ( x, y ) =
            ( round ((x - dx) / cellSize), round ((y - dy) / cellSize) )
    in
    { cellSize = cellSize
    , cellRadius = cellSize / 2
    , toScreen = toScreen
    , toGrid = toGrid
    , width = cellSize * toFloat mem.width
    , height = cellSize * toFloat mem.height
    }


moveCell : { a | toScreen : b -> ( Number, Number ) } -> b -> Shape -> Shape
moveCell cfg pos =
    move (cfg.toScreen pos)


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
