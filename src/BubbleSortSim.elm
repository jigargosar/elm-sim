module BubbleSortSim exposing (main)

import List.Extra
import Playground exposing (..)


numsToString nums =
    List.map String.fromInt nums |> String.join ", "


customSort nums =
    List.sort nums


type alias Model =
    { input : List Int
    , out : List Int
    , len : Int
    , idx : Int
    , wasSwapMade : Bool
    , sorted : Bool
    }


init : List Int -> Model
init nums =
    Model nums nums (List.length nums) 0 False False


step : Model -> Model
step m =
    if m.sorted then
        m

    else if m.idx >= m.len - 1 then
        if m.wasSwapMade then
            { m | idx = 0, wasSwapMade = False }
                |> performSwap

        else
            { m | sorted = True }

    else
        performSwap m


performSwap : Model -> Model
performSwap m =
    let
        newM =
            Maybe.map2
                (\a b ->
                    if a <= b then
                        m

                    else
                        { m | out = List.Extra.swapAt m.idx (m.idx + 1) m.out, wasSwapMade = True }
                )
                (List.Extra.getAt m.idx m.out)
                (List.Extra.getAt (m.idx + 1) m.out)
                |> Maybe.withDefault m
    in
    { newM | idx = m.idx + 1 }


main =
    let
        nums =
            [ 2, 4, 1, 3 ]

        sortedNums =
            List.sort nums

        bubbleSortNums =
            init nums
                |> step
                |> step
                |> step
                |> step
                |> .out

        isCorrect =
            sortedNums == bubbleSortNums

        view _ m =
            [ words black ("Input:  " ++ numsToString nums)
                |> moveUp 20
            , words lightBlue (numsToString sortedNums)
                |> moveDown 20
            , words
                (if isCorrect then
                    green

                 else
                    red
                )
                (numsToString bubbleSortNums)
                |> moveDown 60
            ]
    in
    game view update (init nums)


update _ m =
    m
