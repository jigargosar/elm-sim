module ConnectFourV3Kata2.Main exposing (main)

import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)
import PointFree exposing (is, whenTrue)
import Set exposing (Set)



-- Model


type alias Mem =
    { width : Int
    , height : Int
    , dict : Dict Pos Coin
    , state : State
    , selectedColumn : Int
    }


type alias Pos =
    ( Int, Int )


type Coin
    = Blue
    | Red


type State
    = Turn Coin
    | Victory Coin (Set Pos)
    | Draw


init : Mem
init =
    initWithDim { width = 7, height = 6 }
        |> insertInColumns [ 0, 1, 0, 1, 0, 1, 0, 1, 0 ]
        |> Debug.log "mem"


initWithDim : { a | width : Int, height : Int } -> Mem
initWithDim { width, height } =
    { width = width
    , height = height
    , dict = Dict.empty
    , state = Turn Blue
    , selectedColumn = width // 2
    }



-- Update


insertInColumns columns mem =
    List.foldl insertInColumn mem columns


insertInColumn : Int -> Mem -> Mem
insertInColumn column mem =
    case ( mem.state, insertPosition column mem ) of
        ( Turn coin, Just pos ) ->
            { mem | dict = Dict.insert pos coin mem.dict }
                |> updateState pos coin

        _ ->
            mem


insertPosition : Int -> Mem -> Maybe Pos
insertPosition column mem =
    let
        row =
            Dict.filter (\( x, _ ) _ -> x == column) mem.dict
                |> Dict.size
    in
    if isValidIdx column mem.width && isValidIdx row mem.height then
        Just ( column, row )

    else
        Nothing


isValidIdx : Int -> Int -> Bool
isValidIdx idx len =
    idx >= 0 && idx < len


updateState : Pos -> Coin -> Mem -> Mem
updateState pos coin mem =
    let
        nextState =
            if mem.width * mem.height <= Dict.size mem.dict then
                Draw

            else
                case getWPS pos coin mem of
                    Just wps ->
                        Victory coin wps

                    Nothing ->
                        Turn (flipCoin coin)
    in
    { mem | state = nextState }


getWPS : Pos -> b -> { a | dict : Dict Pos b } -> Maybe (Set Pos)
getWPS sp coin mem =
    let
        stepPos ( dx, dy ) ( x, y ) =
            ( x + dx, y + dy )

        negateStep ( dx, dy ) =
            ( -dx, -dy )

        connectedWithStep ct step pos acc =
            if ct == 0 then
                let
                    _ =
                        Debug.log "crash" ( step, pos, acc )
                in
                Debug.todo "max count reached"

            else
                let
                    nextPos =
                        stepPos step pos
                in
                if Dict.get nextPos mem.dict == Just coin then
                    connectedWithStep (ct - 1)
                        step
                        nextPos
                        (Set.insert nextPos acc)

                else
                    acc

        connectedPositions step =
            connectedWithStep 10 step sp Set.empty
                |> Set.union (connectedWithStep 10 (negateStep step) sp Set.empty)
                |> Set.insert sp
    in
    [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ) ]
        |> List.map connectedPositions
        |> List.Extra.find (Set.size >> is 4)


flipCoin coin =
    case coin of
        Blue ->
            Red

        Red ->
            Blue


update : Computer -> Mem -> Mem
update computer mem =
    let
        { mouse, screen } =
            computer

        cfg =
            toConfig screen mem
    in
    case mem.state of
        Turn _ ->
            let
                column =
                    cfg.toGrid ( mouse.x, 1 ) |> Tuple.first
            in
            if column == mem.selectedColumn then
                whenTrue mouse.click (insertInColumn column) mem

            else
                { mem | selectedColumn = clamp 0 (mem.width - 1) column }

        _ ->
            if mouse.click then
                initWithDim mem

            else
                mem



-- View


type alias Config =
    { cellSize : Float
    , toScreen : Pos -> ( Float, Float )
    , toGrid : ( Float, Float ) -> Pos
    }


toConfig : { a | width : Float, height : Float } -> { b | width : Int, height : Int } -> Config
toConfig screen mem =
    let
        cellSize =
            min ((screen.width * 0.7) / toFloat mem.width)
                ((screen.height * 0.7) / toFloat mem.height)

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
    , toScreen = toScreen
    , toGrid = toGrid
    }


view : Computer -> Mem -> List Shape
view computer mem =
    let
        { screen, time } =
            computer

        cfg =
            toConfig screen mem

        { cellSize } =
            cfg

        moveCell pos =
            move (cfg.toScreen pos)

        cellAt pos =
            Dict.get pos mem.dict

        selectedInsertPos : Maybe Pos
        selectedInsertPos =
            insertPosition mem.selectedColumn mem

        coinToShape coin =
            circle (coin2Color coin) (cellSize * 0.5 * 0.75)

        wps =
            case mem.state of
                Victory _ s ->
                    s

                _ ->
                    Set.empty

        blink : Shape -> Shape
        blink =
            fade (zigzag 0.5 1 1 time)

        highlightIfWP : Pos -> Shape -> Shape
        highlightIfWP pos =
            whenTrue (Set.member pos wps) blink

        viewCellAt : Pos -> Shape
        viewCellAt pos =
            case cellAt pos of
                Just coin ->
                    coinToShape coin
                        |> highlightIfWP pos

                Nothing ->
                    noShape

        indicatorShape : Shape
        indicatorShape =
            case mem.state of
                Turn coin ->
                    let
                        sh =
                            coinToShape coin |> blink
                    in
                    [ case selectedInsertPos of
                        Just pos ->
                            sh |> moveCell pos

                        Nothing ->
                            noShape
                    , sh |> moveCell ( mem.selectedColumn, mem.height )
                    ]
                        |> group

                _ ->
                    noShape
    in
    [ rectangle black (cellSize * toFloat mem.width) (cellSize * toFloat mem.height)
    , mapAllPos
        (\pos ->
            [ circle white (cellSize * 0.5 * 0.9)
            , viewCellAt pos
            ]
                |> group
                |> moveCell pos
        )
        mem
        |> group
    , indicatorShape
    ]


move ( x, y ) =
    Playground.move x y


noShape =
    group []


coin2Color coin =
    case coin of
        Red ->
            red

        Blue ->
            blue


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



-- Main


main =
    game view update init
