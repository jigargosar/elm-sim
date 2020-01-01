module ConnectFourV2.Main exposing (main)

import ConnectFourV2.Board as Board exposing (Board)
import Dict exposing (Dict)
import Playground exposing (..)


type Coin
    = Red
    | Blue


type alias Position =
    ( Int, Int )


type alias Memory =
    Result String Mem


type alias Mem =
    { coin : Coin
    , grid : Dict Position Coin
    , width : Int
    , height : Int
    , board : Board
    }


initialMemory : Memory
initialMemory =
    case Board.empty 7 6 of
        Just a ->
            { coin = Blue
            , grid = Dict.empty
            , width = 7
            , height = 6
            , board = a
            }
                |> Ok

        Nothing ->
            Err "init board failed"



-- makeMove column mem =


updateMemory : Computer -> Memory -> Memory
updateMemory computer mem =
    mem


viewMemory : Computer -> Memory -> List Shape
viewMemory computer model =
    case model of
        Ok mem ->
            [ viewBoard 50 mem.width mem.height (Dict.toList mem.grid) ]

        Err msg ->
            [ words black <| "Error: " ++ msg ]


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
