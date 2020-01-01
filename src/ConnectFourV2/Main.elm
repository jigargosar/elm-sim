module ConnectFourV2.Main exposing (main)

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
    }


initialMemory : Mem
initialMemory =
    let
        _ =
            Dict.Extra.frequencies [ 1, 2, 1, 1, 12, 1, 1 ]
                |> Debug.log "freq"

        _ =
            initBoard 1 1 Blue [ 0, 0 ]
                |> Debug.log "board"
    in
    { coin = Blue, grid = Dict.empty, width = 7, height = 6 }


type ValidBoard
    = ValidBoard Int Int Coin (List Int)


initBoard : Int -> Int -> Coin -> List Int -> Maybe ValidBoard
initBoard w h coin moves =
    let
        foo : ( List Int, List Int )
        foo =
            Dict.Extra.frequencies moves
                |> Dict.toList
                |> List.unzip
                |> Debug.log "foo"

        ( columnIndices, columnLengths ) =
            foo

        isValidIdx len idx =
            idx >= 0 && idx < len

        areMovesValid =
            List.all (isValidIdx w) columnIndices
                && List.all ((+) -1 >> isValidIdx h) columnLengths
    in
    if areMovesValid then
        ValidBoard w h coin moves |> Just

    else
        Nothing



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
