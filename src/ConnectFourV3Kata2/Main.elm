module ConnectFourV3Kata2.Main exposing (main)

import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)
import PointFree exposing (is)
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
    let
        width =
            7
    in
    { width = width
    , height = 6
    , dict = Dict.empty
    , state = Turn Blue
    , selectedColumn = width // 2
    }
        |> insertInColumns [ 0, 1, 0, 1, 0, 1, 0, 1, 0 ]



-- Update


insertInColumns columns mem =
    List.foldl insertInColumn mem columns


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
            if mem.width * mem.height >= Dict.size mem.dict then
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

        connectedWithStep step pos acc =
            let
                nextPos =
                    stepPos step pos
            in
            if Dict.get nextPos mem.dict == Just coin then
                connectedWithStep step nextPos (Set.insert nextPos acc)

            else
                acc

        connectedPositions step =
            connectedWithStep step sp Set.empty
                |> Set.union (connectedWithStep (negateStep step) sp Set.empty)
                |> Set.insert sp
    in
    [ ( 0, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ) ]
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
        { mouse } =
            computer
    in
    case mem.state of
        Turn coin ->
            mem

        _ ->
            if mouse.click then
                init

            else
                mem



-- View


view : Computer -> Mem -> List Shape
view computer mem =
    let
        { screen } =
            computer

        cellSize =
            min ((screen.width * 0.7) / toFloat mem.width)
                ((screen.height * 0.7) / toFloat mem.height)
    in
    [ rectangle black (cellSize * toFloat mem.width) (cellSize * toFloat mem.height)
    ]



-- Main


main =
    game view update init
