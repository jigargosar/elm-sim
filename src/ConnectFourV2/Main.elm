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

                moves =
                    Board.toList mem.board
            in
            [ viewBoard 50 w h (toPositionCoinPairs moves) ]

        Err msg ->
            [ words black <| "Error: " ++ msg ]


toPositionCoinPairs : List Int -> List ( ( Int, Int ), Coin )
toPositionCoinPairs =
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

        reducer : ( Coin, Dict Int Int ) -> Int -> ( ( Coin, Dict Int Int ), ( ( Int, Int ), Coin ) )
        reducer ( coin, lenLookup ) column =
            ( ( Coin.flip coin, incLengthOfColumn column lenLookup )
            , ( ( column, lengthOfColumn column lenLookup ), coin )
            )
    in
    List.Extra.mapAccuml reducer ( Coin.Blue, Dict.empty )
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

        moveCell ( x, y ) =
            let
                leftOffset =
                    -widthPx / 2 + cellSize / 2

                bottomOffset =
                    -heightPx / 2 + cellSize / 2
            in
            move (toFloat x * cellSize + leftOffset)
                (toFloat y * cellSize + bottomOffset)

        toCellBGShape : ( Int, Int ) -> Shape
        toCellBGShape pos =
            circle white (cellSize / 2 * 0.8)
                |> moveCell pos

        toCoinShape : Int -> ( ( Int, Int ), Coin ) -> Shape
        toCoinShape idx ( position, coin ) =
            [ circle white (cellSize / 2 * 0.8)
            , circle (coinToColor coin) (cellSize / 2 * 0.7)
            , words white (String.fromInt idx)
            ]
                |> group
                |> moveCell position
    in
    group
        [ rectangle black widthPx heightPx
        , mapPositionsFromWH w h toCellBGShape |> group
        , List.indexedMap toCoinShape list |> group
        ]


toPositions : Int -> Int -> List ( Int, Int )
toPositions w h =
    List.Extra.initialize w
        (\x -> List.Extra.initialize h (\y -> ( x, y )))
        |> List.concat


mapPositionsFromWH : Int -> Int -> (( Int, Int ) -> b) -> List b
mapPositionsFromWH w h func =
    toPositions w h |> List.map func


main =
    game viewMemory updateMemory initialMemory
