module MirrorPuzzleV3.Main exposing (main)

import Collage exposing (circle, filled, rectangle, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
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


collageDemo =
    let
        circ =
            circle 50
                |> filled (uniform Color.red)

        rect =
            rectangle 200 100
                |> filled (uniform Color.blue)
    in
    rect
        |> at topLeft circ
        |> svg
