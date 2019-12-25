module BubbleSortSim exposing (main)

import Playground exposing (..)


viewNums nums =
    let
        strRep =
            List.map String.fromInt nums |> String.join ", "
    in
    words black <| strRep


customSort nums =
    List.sort nums


main =
    let
        nums =
            [ 2, 4, 1, 3 ]

        sortedNums =
            List.sort nums

        quickSortNums =
            customSort nums
    in
    picture
        [ viewNums nums
            |> moveUp 20
        , viewNums sortedNums
            |> moveDown 20
        ]
