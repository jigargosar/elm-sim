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
    case Board.initWithMoves 7 6 [ 0, 1, 0, 1 ] of
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


toVM : Board -> List ( ( Int, Int ), Color )
toVM =
    let
        flipColor color =
            if color == red then
                blue

            else
                red

        reducer column ( color, dict ) =
            ( flipColor color
            , Dict.update column
                (\v ->
                    case v of
                        Nothing ->
                            List.singleton color |> Just

                        Just list ->
                            color :: list |> Just
                )
                dict
            )
    in
    Board.foldl reducer ( blue, Dict.empty )
        >> Tuple.second
        >> Dict.toList
        >> List.concatMap
            (\( x, coins ) ->
                List.indexedMap (\y -> Tuple.pair ( x, y )) coins
            )


viewBoard : Float -> Int -> Int -> List ( ( Int, Int ), Color ) -> Shape
viewBoard cellSize w h list =
    let
        ( widthPx, heightPx ) =
            ( toFloat w * cellSize, toFloat h * cellSize )

        moveCell x y =
            move (toFloat x * cellSize) (toFloat y * cellSize)

        viewColumn : Int -> ( ( Int, Int ), Color ) -> Shape
        viewColumn idx ( ( x, y ), color ) =
            [ circle
                color
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
