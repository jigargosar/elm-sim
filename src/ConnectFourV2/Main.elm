module ConnectFourV2.Main exposing (main)

import ConnectFourV2.Board as Board exposing (Board)
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
            in
            [ viewBoard 50 w h (toVM mem.board) ]

        Err msg ->
            [ words black <| "Error: " ++ msg ]


toVM : Board -> List ( ( Int, Int ), Coin )
toVM =
    let
        reducer column ( coin, dict ) =
            ( Coin.flip coin
            , Dict.update column
                (\v ->
                    case v of
                        Nothing ->
                            List.singleton coin |> Just

                        Just list ->
                            coin :: list |> Just
                )
                dict
            )
    in
    Board.foldl reducer ( Coin.Blue, Dict.empty )
        >> Tuple.second
        >> Dict.toList
        >> List.concatMap
            (\( x, coins ) ->
                List.indexedMap (\y -> Tuple.pair ( x, y )) coins
            )


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Coin.Blue ->
            blue

        Coin.Red ->
            red


viewBoard : Float -> Int -> Int -> List ( ( Int, Int ), Coin ) -> Shape
viewBoard cellSize w h list =
    let
        ( widthPx, heightPx ) =
            ( toFloat w * cellSize, toFloat h * cellSize )

        moveCell x y =
            move (toFloat x * cellSize) (toFloat y * cellSize)

        viewColumn : Int -> ( ( Int, Int ), Coin ) -> Shape
        viewColumn idx ( ( x, y ), coin ) =
            [ circle
                (coinToColor coin)
                (cellSize / 2 * 0.7)
            , words white (String.fromInt idx)
            ]
                |> group
                |> moveCell x y

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
        , List.indexedMap viewColumn list |> groupGridCells
        ]


main =
    game viewMemory updateMemory initialMemory
