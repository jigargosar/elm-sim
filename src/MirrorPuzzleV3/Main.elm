module MirrorPuzzleV3.Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.ElGrid as ElGrid
import MirrorPuzzleV3.TileGridDemo as TileGridDemo



-- Main


main =
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "TileGrid" ]
            , TileGridDemo.viewDemo
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Grid" ]
            , ElGrid.viewDemo
            ]
        ]
