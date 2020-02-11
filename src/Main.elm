module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (class)


type Shape
    = Square
    | Line
    | Tee
    | S


main =
    let
        viewSquare =
            """
            0000
            0110
            0110
            0000
            """
                |> String.trim
                |> String.split "\n"
                |> List.map String.trim
                |> String.join "\n"
    in
    div [ class "mono-board" ]
        [ text viewSquare
        ]
