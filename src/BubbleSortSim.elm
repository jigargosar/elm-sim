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
    , elapsed : Int
    }


init : List Int -> Model
init nums =
    Model nums nums (List.length nums) 0 False False 0


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
    in
    game view update (init nums)


update _ m =
    let
        interval =
            60
    in
    (if modBy interval m.elapsed == interval - 1 then
        step m

     else
        m
    )
        |> tick


tick m =
    { m | elapsed = m.elapsed + 1 }


view _ m =
    let
        sortedNums =
            List.sort m.input

        isCorrect =
            sortedNums == m.out
    in
    [ words black ("Input:  " ++ numsToString m.input)
        |> moveUp 20
    , words lightBlue (numsToString sortedNums)
        |> moveDown 20
    , words
        (if isCorrect then
            green

         else
            red
        )
        (numsToString m.out)
        |> moveDown 60
    ]
