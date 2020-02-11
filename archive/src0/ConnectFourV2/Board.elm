module ConnectFourV2.Board exposing (Board, empty, initWithMoves, move, toList, wh)

import Dict
import Dict.Extra


type Board
    = Board Int Int (List Int)


initWithMoves : Int -> Int -> List Int -> Maybe Board
initWithMoves w h moves =
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
        Board w h moves |> Just

    else
        Nothing


move : Int -> Board -> Maybe Board
move column (Board w h moves) =
    initWithMoves w h (moves ++ [ column ])


empty : Int -> Int -> Maybe Board
empty w h =
    if w <= 0 || h <= 0 then
        Nothing

    else
        Board w h [] |> Just


toList : Board -> List Int
toList (Board _ _ moves) =
    moves


wh : Board -> ( Int, Int )
wh (Board w h _) =
    ( w, h )
