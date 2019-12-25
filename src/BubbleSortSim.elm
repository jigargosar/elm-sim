module BubbleSortSim exposing (..)

import Playground exposing (..)


viewNums nums =
    let
        strRep =
            List.map String.fromInt nums |> String.join ", "
    in
    words black <| strRep


main =
    let
        nums =
            [ 1, 2, 3, 4 ]
    in
    picture
        [ viewNums nums
            |> moveUp 20
        , viewNums nums
            |> moveDown 20
        ]
