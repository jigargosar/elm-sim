module ConnectFourV2.BoardShape exposing (toAllCells, toBoardShape)

import ConnectFourV2.Coin as Coin exposing (Coin)
import Dict exposing (Dict)
import List.Extra
import Playground exposing (..)


toAllCells : Int -> Int -> List Int -> List ( ( Int, Int ), Maybe Coin )
toAllCells w h =
    toPositionCoinPairs
        >> Dict.fromList
        >> (\dict ->
                mapPositionsFromWH w
                    h
                    (\pos ->
                        ( pos, Dict.get pos dict )
                    )
           )


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


toPositions : Int -> Int -> List ( Int, Int )
toPositions w h =
    List.Extra.initialize w
        (\x -> List.Extra.initialize h (\y -> ( x, y )))
        |> List.concat


mapPositionsFromWH : Int -> Int -> (( Int, Int ) -> b) -> List b
mapPositionsFromWH w h func =
    toPositions w h |> List.map func



-- View


coinToColor : Coin -> Color
coinToColor coin =
    case coin of
        Coin.Blue ->
            blue

        Coin.Red ->
            red


toBoardShape : Float -> Int -> Int -> List ( ( Int, Int ), Maybe Coin ) -> Shape
toBoardShape cellSize w h list =
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

        toCellShape : Int -> ( ( Int, Int ), Maybe Coin ) -> Shape
        toCellShape idx ( position, cell ) =
            (circle white (cellSize / 2 * 0.8)
                :: (case cell of
                        Just coin ->
                            [ circle (coinToColor coin) (cellSize / 2 * 0.7)
                            , words white (String.fromInt idx)
                            ]

                        Nothing ->
                            []
                   )
            )
                |> group
                |> moveCell position
    in
    group
        [ rectangle black widthPx heightPx
        , List.indexedMap toCellShape list |> group
        ]
