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
        incLengthOfColumn : Int -> Dict Int Int -> Dict Int Int
        incLengthOfColumn column =
            Dict.update column <|
                \maybeLength ->
                    case maybeLength of
                        Just length ->
                            Just (length + 1)

                        Nothing ->
                            Just 1

        lengthOfColumn : Int -> Dict Int Int -> Int
        lengthOfColumn column lookup =
            Dict.get column lookup |> Maybe.withDefault 0

        reducer column ( ( coin, lenLookup ), acc ) =
            ( ( Coin.flip coin, incLengthOfColumn column lenLookup )
            , ( ( column, lengthOfColumn column lenLookup ), coin ) :: acc
            )
    in
    Board.toList
        >> List.foldl reducer ( ( Coin.Blue, Dict.empty ), [] )
        >> Tuple.second
        >> List.reverse



-- >> Dict.toList


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
