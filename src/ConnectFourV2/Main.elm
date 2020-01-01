module ConnectFourV2.Main exposing (main)

import ConnectFourV2.Board as Board exposing (Board)
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
    case Board.initWithMoves 7 6 [ 0, 0, 0 ] of
        Just a ->
            { board = a
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
            let
                ( w, h ) =
                    Board.wh mem.board
            in
            [ viewBoard 50 w h (toVM mem.board) ]

        Err msg ->
            [ words black <| "Error: " ++ msg ]


toVM : Board -> List ( Int, List Bool )
toVM =
    let
        reducer column ( bool, dict ) =
            ( not bool
            , Dict.update column
                (\v ->
                    case v of
                        Nothing ->
                            List.singleton bool |> Just

                        Just list ->
                            bool :: list |> Just
                )
                dict
            )
    in
    Board.foldl reducer ( True, Dict.empty )
        >> Tuple.second
        >> Dict.toList


viewBoard : Float -> Int -> Int -> List ( Int, List Bool ) -> Shape
viewBoard cellSize w h list =
    let
        ( widthPx, heightPx ) =
            ( toFloat w * cellSize, toFloat h * cellSize )

        viewCoin bool =
            circle
                (if bool then
                    blue

                 else
                    red
                )
                (cellSize / 2 * 0.7)

        moveCell x y =
            move (toFloat x * cellSize) (toFloat y * cellSize)

        viewColumn : ( Int, List Bool ) -> List Shape
        viewColumn ( x, coins ) =
            List.indexedMap (\y -> viewCoin >> moveCell x y) coins

        groupGridCells cellShapes =
            cellShapes
                |> group
                |> moveLeft (widthPx / 2 - cellSize / 2)
                |> moveDown (heightPx / 2 - cellSize / 2)
    in
    group
        [ rectangle black widthPx heightPx
        , List.Extra.initialize w
            (\x ->
                List.Extra.initialize h
                    (\y ->
                        circle white (cellSize / 2 * 0.8)
                            |> moveCell x y
                    )
            )
            |> List.concat
            |> groupGridCells
        , List.concatMap viewColumn list |> groupGridCells
        ]


main =
    game viewMemory updateMemory initialMemory
