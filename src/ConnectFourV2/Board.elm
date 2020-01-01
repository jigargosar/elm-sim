module ConnectFourV2.Board exposing (Board, empty, initBoard)

import Dict
import Dict.Extra


type Board
    = Board Int Int (List Int)


initBoard : Int -> Int -> List Int -> Maybe Board
initBoard w h moves =
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


empty : Int -> Int -> Maybe Board
empty w h =
    if w <= 0 || h <= 0 then
        Nothing

    else
        Board w h [] |> Just
