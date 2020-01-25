module MirrorPuzzleV3.Main exposing (main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.ElGrid as ElGrid
import MirrorPuzzleV3.TileGridDemo as TileGridDemo



-- Main


main =
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "collageDemo" ]
            , Html.div [ class "pa4" ] [ collageDemo ]
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "TileGrid" ]
            , TileGridDemo.viewDemo
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Grid" ]
            , ElGrid.viewDemo
            ]
        ]


collageDemo : Html msg
collageDemo =
    let
        cell =
            square 50
                -- |> filled (uniform Color.blue)
                |> outlined (solid thin (uniform Color.red))

        repeatSpaced n =
            List.repeat n >> List.intersperse (spacer 10 10)

        gridCells =
            repeatSpaced 5 cell
                |> horizontal
                |> repeatSpaced 10
                |> vertical
                |> align base

        gridBackground =
            square 250
                |> filled (uniform Color.red)
                |> opacity 0.1
                |> align base
    in
    impose
        (gridCells
            |> align bottomLeft
            |> shift ( 20, 20 )
        )
        (gridBackground |> align bottomLeft)
        |> svg
