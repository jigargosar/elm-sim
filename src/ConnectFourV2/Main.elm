module ConnectFourV2.Main exposing (main)

import ConnectFourV2.Board as Board exposing (Board)
import ConnectFourV2.BoardShape as BoardShape
import ConnectFourV2.Coin as Coin exposing (Coin)
import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)


type alias Position =
    ( Int, Int )


type alias Memory =
    Result String Mem


type alias Mem =
    { board : Board
    }


initialMemory : Memory
initialMemory =
    case Board.initWithMoves 7 6 [ 0, 1, 0, 1 ] of
        Just a ->
            { board = a
            }
                |> Ok

        Nothing ->
            Err "init board failed"



-- makeMove column mem =


updateMemory : Computer -> Memory -> Memory
updateMemory _ mem =
    mem


viewMemory : Computer -> Memory -> List Shape
viewMemory _ model =
    case model of
        Ok mem ->
            let
                ( w, h ) =
                    Board.wh mem.board

                moves =
                    Board.toList mem.board
            in
            [ BoardShape.toBoardShape 50 w h mem.board ]

        Err msg ->
            [ words black <| "Error: " ++ msg ]



-- ViewModel


main =
    game viewMemory updateMemory initialMemory
