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
        rect =
            square 50
                -- |> filled (uniform Color.blue)
                |> outlined (solid thin (uniform Color.red))

        cells =
            List.repeat 5 rect
                |> List.intersperse (square 10 |> filled transparent)

        row =
            cells |> horizontal

        gridCells =
            List.repeat 5 row
                |> List.intersperse (square 10 |> filled transparent)
                |> vertical
                |> align base
                |> shift ( 70, -100 )

        gridBackground =
            square 250
                |> filled (uniform Color.red)
                |> opacity 0.1
                |> align base
    in
    impose gridCells gridBackground
        |> svg
