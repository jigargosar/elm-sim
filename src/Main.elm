module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Set


main =
    div []
        [ viewLineGrid
        , viewBoardGrid
        ]


viewGridCells : Int -> Int -> Grid () -> Html msg
viewGridCells w h grid =
    List.range 0 (h - 1)
        |> List.map
            (\y ->
                List.range 0 (w - 1)
                    |> List.map
                        (\x ->
                            case get x y grid of
                                Ok (Just _) ->
                                    viewOn

                                Ok Nothing ->
                                    viewOff

                                Err _ ->
                                    viewError
                        )
            )
        |> List.map (div [ style "display" "flex" ])
        |> div
            [ wsPre
            , fontMono
            , fz "30px"
            , pa "10px"
            , style "line-height" "1"
            ]


viewOn =
    div
        [ style "width" "30px"
        , style "height" "30px"
        , style "background-color" "black"
        , style "opacity" "0.8"
        , style "margin" "1px"
        ]
        []


viewOff =
    div
        [ style "width" "30px"
        , style "height" "30px"
        , style "background-color" "black"
        , style "opacity" "0.2"
        , style "margin" "1px"
        ]
        []


viewError =
    div
        [ style "width" "30px"
        , style "height" "30px"
        , style "background-color" "red"
        , style "opacity" "0.8"
        , style "margin" "1px"
        ]
        []



-- Board


boardGrid : Grid ()
boardGrid =
    empty 9 18


viewBoardGrid =
    viewGridCells 9 18 boardGrid



-- Line


lineGrid : Grid ()
lineGrid =
    empty 4 4
        |> set 1 0 ()
        |> Result.andThen (set 1 1 ())
        |> Result.andThen (set 1 2 ())
        |> Result.andThen (set 1 3 ())
        |> Result.withDefault (empty 4 4)


viewLineGrid =
    viewGridCells 4 4 lineGrid


wsPre =
    style "white-space" "pre"


fontMono =
    style "font-family" "monospace"


fz =
    style "font-size"


pa =
    style "padding"



-- GRID


type Grid a
    = Grid Int Int (Dict ( Int, Int ) a)


type Error
    = OutOfBounds


empty : Int -> Int -> Grid a
empty w h =
    Grid w h Dict.empty


set : Int -> Int -> a -> Grid a -> Result Error (Grid a)
set x y a (Grid w h dict) =
    if x >= 0 && y >= 0 && x <= w && y <= h then
        Ok (Grid w h (Dict.insert ( x, y ) a dict))

    else
        Err OutOfBounds


get : Int -> Int -> Grid a -> Result Error (Maybe a)
get x y (Grid w h dict) =
    if x >= 0 && y >= 0 && x <= w && y <= h then
        Ok (Dict.get ( x, y ) dict)

    else
        Err OutOfBounds
