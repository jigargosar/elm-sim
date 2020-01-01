module ConnectFourV2.Main exposing (Board, Coin(..), initBoard)

import Array exposing (Array)
import Dict exposing (Dict)
import Dict.Extra
import List.Extra
import Playground exposing (..)


type Coin
    = Red
    | Blue


type alias Position =
    ( Int, Int )


type alias Mem =
    { coin : Coin
    , grid : Dict Position Coin
    , width : Int
    , height : Int
    , board : Board
    }


initialMemory : Mem
initialMemory =
    let
        _ =
            Dict.Extra.frequencies [ 1, 2, 1, 1, 12, 1, 1 ]
                |> Debug.log "freq"

        _ =
            initBoard 1 1 [ 0, 0 ]
                |> Debug.log "board"
    in
    { coin = Blue
    , grid = Dict.empty
    , width = 7
    , height = 6
    , board = emptyBoard 7 6
    }


type Board
    = Board Int Int (List Int)


initBoard : Int -> Int -> List Int -> Maybe Board
initBoard w h moves =
    let
        ( columnIndices, columnLengths ) =
            Dict.Extra.frequencies moves
                |> Dict.toList
                |> List.unzip

        isValidIdx len idx =
            idx >= 0 && idx < len

        areMovesValid =
            List.all (isValidIdx w) columnIndices
                && List.all ((+) -1 >> isValidIdx h) columnLengths
    in
    if areMovesValid then
        Board w h Blue moves |> Just

    else
        Nothing


emptyBoard : Int -> Int -> Board
emptyBoard w h =
    Board w h []



-- makeMove column mem =


updateMemory : Computer -> Mem -> Mem
updateMemory computer mem =
    mem


viewMemory : Computer -> Mem -> List Shape
viewMemory computer mem =
    [ viewBoard 50 mem.width mem.height (Dict.toList mem.grid) ]


viewBoard : Float -> Int -> Int -> List ( Position, Coin ) -> Shape
viewBoard cellSize w h list =
    let
        ( widthPx, heightPx ) =
            ( toFloat w * cellSize, toFloat h * cellSize )
    in
    group
        [ rectangle black widthPx heightPx
        ]


main =
    game viewMemory updateMemory initialMemory
